(ns adzerk.zerkdown.parser
  (:refer-clojure :exclude [cat])
  (:require
    [clojure.java.io        :as io]
    [clojure.string         :as string]
    [instaparse.core        :as insta]
    [instaparse.combinators :as combi :refer [string cat alt ebnf nt hide-tag]]))

(def grammar "adzerk/zerkdown/grammar.ebnf")
(def rules   (->> grammar io/resource slurp ebnf))

(def transform-map
  {:STRING                      str
   :MAP                         str
   :VEC                         str
   :LIST                        str
   :LINE                        str
   :PRE-LINE                    str
   :BLANK-LINE                  str
   :NONBLANK-LINE               str
   :NONBLANK-PRE-LINE           str
   :BLOCK-TAG-IN                str
   :PRE-BLOCK-TAG-IN            str
   :DATA                        str
   :BLOCK-TAG                   vector
   :PRE-BLOCK-TAG               vector
   :BLOCK-BODY                  str
   :PRE-BLOCK-BODY              str
   :TEXT                        str
   :BLOCKS                      vector
   :PRE-BLOCK                   #(vector :BLOCK %1 [%2])})

(defn concat-text
  [blocks]
  (if-not (vector? blocks)
    blocks
    (-> #(if-not (every? string? [(peek %1) %2])
           (conj %1 %2)
           (conj (pop %1) (str (peek %1) %2)))
        (reduce [] blocks))))

(defn make-rules
  [block-tags pre-block-tags indent]
  (merge rules {:BLOCK-TAG-IN     (apply alt (map string block-tags))
                :PRE-BLOCK-TAG-IN (apply alt (map string pre-block-tags))
                :INDENT           (hide-tag (apply cat (repeat indent (nt :SP))))}))

(defn make-parser
  [block-tags pre-block-tags indent start-rule]
  (let [a (atom nil)
        r (make-rules block-tags pre-block-tags indent)]
    (reset! a (comp concat-text
                    (->> #(vector :BLOCK %1 (@a %2))
                         (assoc transform-map :BLOCK)
                         (partial insta/transform))
                    (insta/parser r :start start-rule)))))
