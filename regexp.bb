#!/usr/bin/env bb
;; Babashka backend for visual-regexp-steroids (vr/engine 'custom).
;; Same stdin/stdout protocol as regexp.py, but java.util.regex and Clojure eval.
(ns vr-regexp
  (:import [java.util Base64]
           [java.util.regex Matcher Pattern]))

(defn escape [s]
  (.encodeToString (Base64/getEncoder) (.getBytes (str s) "UTF-8")))

(defn message [msg]
  (println (escape msg)))

(def bool-flags #{"--eval" "--feedback" "--backwards"})
(def value-flags #{"--regexp" "--replace" "--feedback-limit" "--flags"})

(def pattern-flags
  "u is a no-op: like Python 3, classes such as \\w are always Unicode-aware."
  {\i Pattern/CASE_INSENSITIVE
   \m Pattern/MULTILINE
   \s Pattern/DOTALL
   \u 0})

(defn compile-regexp [{:keys [regexp flags]}]
  (Pattern/compile regexp (reduce bit-or Pattern/UNICODE_CHARACTER_CLASS (map pattern-flags flags))))

(defn parse-args
  "Unknown tokens are skipped: the elisp passes a literal \"nil\" when there is no limit."
  [args]
  (loop [[a & more] args
         opts {}]
    (cond
      (nil? a) opts
      (bool-flags a) (recur more (assoc opts (keyword (subs a 2)) true))
      (value-flags a) (recur (rest more) (assoc opts (keyword (subs a 2)) (first more)))
      :else (recur more opts))))

(defn cp
  "Java indices are UTF-16 units; Emacs positions are code points."
  [^String s idx]
  (if (neg? idx) idx (.codePointCount s 0 idx)))

(defn snapshot
  "Plain-data view of the matcher's current match: :groups, :spans (code points), :named."
  [^String region ^Matcher m names]
  (let [n (inc (.groupCount m))]
    {:groups (mapv #(.group m (int %)) (range n))
     :spans (mapv #(vector (cp region (.start m (int %))) (cp region (.end m (int %)))) (range n))
     :named (update-vals names #(.group m (int %)))}))

(defn mid-surrogate? [^String s i]
  (and (< 0 i (.length s)) (Character/isLowSurrogate (.charAt s (int i)))))

(defn match-seq
  "Empty matches advance one UTF-16 unit, which can land inside a surrogate pair; drop those."
  [^Pattern p ^String region]
  (let [m (.matcher p region)
        names (into {} (.namedGroups p))]
    (->> (repeatedly #(when (.find m)
                        (if (mid-surrogate? region (.start m)) ::skip (snapshot region m names))))
         (take-while some?)
         (remove #{::skip}))))

(defn expand
  "Python-style template: \\1 and \\g<name> are groups, everything else is literal."
  [{:keys [groups named]} template]
  (clojure.string/replace
   template
   #"\\(\d+)|\\g<(\w+)>|\\\\"
   (fn [[_ n nm]] (cond n (str (groups (parse-long n)))
                        nm (str (named nm))
                        :else "\\"))))

(defn eval-replacer
  "Returns (fn [match i] -> replacement) for --eval mode.
  `match` is a snapshot map (:groups, :spans, :named), `i` is the 0-based match counter."
  [replace]
  (let [src (-> replace
                (clojure.string/replace #"\\(\d+)" "(get-in m [:groups $1])")
                (clojure.string/replace #"\\g<(\w+)>" "(get-in m [:named \"$1\"])"))
        form (read-string src)]
    (binding [*ns* (create-ns 'vr-eval)]
      (eval '(clojure.core/refer-clojure))
      (eval '(require '[clojure.string :as str]))
      (eval (list 'fn '[m i] form)))))

(defn run-matches [{:keys [backwards feedback-limit] :as opts} region]
  (let [ms (cond-> (vec (match-seq (compile-regexp opts) region))
             backwards rseq)]
    (doseq [mr (cond->> ms feedback-limit (take (parse-long feedback-limit)))]
      (println (clojure.string/join " " (map #(clojure.string/join " " %) (:spans mr)))))
    (message (if (seq ms) (format "%d matches" (count ms)) "no match"))))

(defn run-replace [{:keys [replace eval feedback feedback-limit] :as opts} region]
  (let [ms (vec (match-seq (compile-regexp opts) region))
        render (if eval (eval-replacer replace) (fn [mr _] (expand mr replace)))
        shown (cond->> ms (and feedback feedback-limit) (take (parse-long feedback-limit)))]
    (doseq [[i mr] (map-indexed vector shown)]
      (println (format "%d %d %s"
                       (get-in mr [:spans 0 0]) (get-in mr [:spans 0 1])
                       (escape (render mr i)))))
    (message (cond
               (not feedback) (format "replaced %d matches" (count ms))
               (seq ms) (format "%d matches" (count ms))
               :else "no match"))))

(let [[cmd & args] *command-line-args*
      opts (parse-args args)
      region (slurp *in*)]
  (try
    (case cmd
      "matches" (run-matches opts region)
      "replace" (run-replace opts region))
    (catch Exception e
      (message (str "Invalid: " (first (clojure.string/split-lines (str (ex-message e)))))))))
