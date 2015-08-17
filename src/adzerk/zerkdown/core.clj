(ns adzerk.zerkdown.core
  (:refer-clojure :exclude [subs])
  (:require
    [clojure.java.io :as io]
    [instaparse.core :as insta]
    [instaparse.combinators :as i]
    [clojure.set     :as set    :refer [union]]
    [clojure.string  :as string :refer [blank? split join triml trimr]])
  (:import
    [java.io PushbackReader BufferedReader StringReader]))

(def BLOCK
  #{"#" "."})

(def BLOCK-PRE
  #{"```"})

(def INLINE
  {"<" ">"})

(def INLINE-PRE
  #{"`"})

(def LIST-ITEM
  #{"*"})

(def DEFINITION-LIST-ITEM
  {"@" ":"})

(def INDENT
  "  ")

(declare parse)

(defn orfn      [x y] (or x y))
(defn sw?       [s t] (.startsWith s t))
(defn pbr       [s]   (PushbackReader. (StringReader. s)))
(defn subs      [& x] (try (apply clojure.core/subs x) (catch Throwable _)))
(defn indented? [s]   (or (blank? s) (sw? s INDENT)))
(defn indent    [s]   (if (blank? s) s (str INDENT (triml s))))
(defn dedent    [s]   (when (sw? s INDENT) (subs s (count INDENT))))
(defn pre?      [t]   (or (BLOCK-PRE t) (contains? INLINE-PRE t)))

(defn read-string-1
  [x]
  (when x
    (prn :x x)
    (with-open [r (pbr x)]
      [(read r false nil) (slurp r)])))

(defn parse-tag
  [tag input]
  (when input
    (let [t1?  (sw? input (str tag " "))
          t2?  (sw? input (str tag "{"))
          body (subs input (count tag))]
      (cond t1? [nil body]
            t2? (read-string-1 body)))))

(defn parse-tags
  [tags input]
  (some #(parse-tag % input) (seq tags)))

(defn parse-block-tag
  [tag input]
  (when input
    (prn :X input)
    (let [[args body] (parse-tag tag input)]
      (when body
        (let [[l & ls]     (split body #"\n")
              lines        (concat (if (blank? l) [] [(indent l)]) ls)
              [body input] (split-with indented? lines)
              contents     (let [in (join "\n" (drop-while blank? (map dedent body)))]
                             (if (pre? tag) [(trimr in)] (parse in)))
              input        (triml (join "\n" input))]
          {:tag tag :attr args :kids contents :input input})))))

(defn parse-list-tag
  [tag input]
  (loop [saved input ret nil x (parse-block-tag tag input)]
    (if-not x
      (when ret (assoc ret :type :list :input saved))
      (recur (:input x)
             (update-in ret [:kids] (fnil conj []) (dissoc x :input))
             (parse-block-tag tag (:input x))))))

(defn parse-block-tags
  [tags input]
  (some #(parse-block-tag % input) (seq tags)))

(defn parse-list-tags
  [tags input]
  (some #(parse-list-tag % input) (seq tags)))

;;(defn next-inline-tag-index
;;  [tags input]
;;  (reduce min (map #(.indexOf input %) (seq tags))))
;;
;;(defn parse-next-inline-tags
;;  [tags input]
;;  (let [i (next-inline-tag-index (keys tags) input)]
;;    (cond (pos? i)  [(subs input 0 i) (subs input i)]
;;          (zero? i) (let [j (next-inline-tag-index (vals tags) input)]
;;                      ))))
;;
;;(defn unterminated
;;  [tag input]
;;  (throw (ex-info ("Unterminated inline tag: " (key tag)) {:input input})))
;;
;;(defn parse-inline-tag
;;  [tag input]
;;  
;;  )
;;
;;(defn parse-inline-tags-1
;;  [tags input]
;;  (or (let [[args body :as parsed] (parse-tag (key tag) input)]
;;        (when parsed
;;          (loop [ret nil body body]
;;            (cond (not c)              (unterminated tag input)
;;                  (sw? body (val tag)) [{:tag (key tag) :attr args :kids}]))
;;          ))
;;      (parse-text-tags tags input)))

(defn parse-inline-raw-tags
  [tags input]
  (when-not (blank? input)
    (let [lines    (split input #"\n")
          contents (take-while #(nil? (parse-tags tags %)) lines)
          input    (triml (join "\n" (drop (count contents) lines)))]
      (when (seq contents)
        [(join "\n" contents) input]))))

(defn parse-inline-tags
  [tags input]
  (when-not (blank? input)
    (let [[raw input] (parse-inline-raw-tags tags input)]
      (when raw
        {:inline true :kids [raw] :input input}))))

(defn parse-1
  [input]
  (or (parse-list-tags LIST-ITEM input)
      (parse-block-tags (union BLOCK BLOCK-PRE) input)
      (parse-inline-tags (union LIST-ITEM BLOCK BLOCK-PRE) input)))

(defn parse
  [input]
  (loop [ret nil {:keys [input] :as x} (parse-1 input)]
    (if-not x
      ret
      (let [y (if (:inline x) (:kids x) (dissoc x :input))]
        (recur ((comp vec flatten (fnil conj [])) ret y) (parse-1 input))))))

(comment

  (->> "test.txt" slurp parse)

  )
