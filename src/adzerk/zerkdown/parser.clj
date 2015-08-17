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
  {:TAGGED-BLOCK-TAG-IN
   {:tag :alt :parsers (map #(hash-map :tag :string :string %) strings)}})

(defn make-rules
  [block-tags]
  (merge rules (make-block-tag-rule block-tags)))

(defn prep-input
  [s]
  (str s "\n"))

(def make-parser
  (memoize
    (fn [block-tags start-rule]
      (comp (partial insta/transform transform-map)
            (insta/parser (make-rules block-tags) :start start-rule)))))

#_(defn parse-blocks
  [block-tags input]
  (let [parse (make-parser block-tags :BLOCKS)]
    ))
