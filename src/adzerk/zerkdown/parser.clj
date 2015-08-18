(ns adzerk.zerkdown.parser
  (:require
    [clojure.java.io        :as io]
    [clojure.string         :as string]
    [instaparse.core        :as insta]
    [instaparse.combinators :as combi]))

(def grammar "adzerk/zerkdown/grammar.ebnf")
(def rules   (->> grammar io/resource slurp combi/ebnf))

(def transform-map
  {:STRING                    str
   :MAP                       str
   :VEC                       str
   :LINE                      str
   :BLANK-LINE                str
   :NONBLANK-LINE             str
   :TAGGED-BLOCK-TAG-IN       str
   :CLJ                       str
   :TAGGED-BLOCK-TAG          vector
   :TAGGED-BLOCK-BODY         str
   :TEXT-BLOCK                str
   :BLOCKS                    vector})

(defn make-block-tag-rule
  [strings]
  {:TAGGED-BLOCK-TAG-IN (apply combi/alt (map combi/string strings))})

(defn make-rules
  [block-tags]
  (merge rules (make-block-tag-rule block-tags)))

(defn prep-input
  [s]
  (str s "\n"))

(defn make-transform
  [m]
  (partial insta/transform m))

(defn fuse-text-blocks
  [blocks]
  (if-not (vector? blocks)
    blocks
    (-> #(if-not (every? string? [(peek %1) %2])
           (conj %1 %2)
           (conj (pop %1) (str (peek %1) %2)))
        (reduce [] blocks))))

(defn make-parser
  [block-tags start-rule]
  (let [a (atom nil)
        r (make-rules block-tags)]
    (reset! a (comp fuse-text-blocks
                    (->> #(vector :TAGGED-BLOCK %1 (@a %2))
                         (assoc transform-map :TAGGED-BLOCK)
                         (partial insta/transform))
                    (insta/parser (make-rules block-tags) :start start-rule)))))
