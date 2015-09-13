(ns adzerk.zerkdown.parser
  (:refer-clojure :exclude [cat])
  (:require
    [clojure.java.io        :as io]
    [clojure.string         :as string]
    [instaparse.core        :as insta]
    [instaparse.combinators :as combi :refer [string cat alt ebnf nt hide-tag]]))

(def grammar "adzerk/zerkdown/grammar.ebnf")
(def rules   (->> grammar io/resource slurp ebnf))

(defn splice [[tag items :as form]]
  (if (= tag :SPLICE) items [form]))

(defn part-list
  [[acc-tag acc-kids] [_ [tag] :as item]]
  (let [[head [tag* :as attr] kids :as xx] (peek acc-kids)]
    (if (= tag tag*)
      (->> [head attr (conj kids item)]
           (conj (pop acc-kids))
           (vector acc-tag))
      (->> (-> acc-kids (conj [:LIST [tag] [item]]))
           (conj [acc-tag])))))

(defn concat-text
  [blocks]
  (if-not (vector? blocks)
    blocks
    (-> #(if-not (every? string? [(peek %1) %2])
           (conj %1 %2)
           (conj (pop %1) (str (peek %1) %2)))
        (reduce [] blocks))))

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
   :LIST-BLOCK-TAG-IN           str
   :DATA                        str
   :BLOCK-TAG                   vector
   :PRE-BLOCK-TAG               vector
   :LIST-BLOCK-TAG              vector
   :BLOCK-BODY                  str
   :PRE-BLOCK-BODY              str
   :TEXT-BODY                   #(vector :SPLICE (vec %&))
   :NOT-INLINE                  str
   :INLINE-DATA                 vector
   :BLOCKS                      #(vec (mapcat splice %&))
   :LIST-BLOCKS                 #(reduce part-list [:SPLICE []] %&)
   :PRE-BLOCK                   #(vector :BLOCK %1 [%2])
   :INLINE                      #(let [[open & more] %&
                                       [[data] close & chars] (reverse more)]
                                   [:INLINE [open close data] (concat-text (vec (reverse chars)))])})

(defn make-rules
  [block-tags pre-block-tags list-block-tags indent]
  (merge rules {:BLOCK-TAG-IN      (apply alt (map string block-tags))
                :PRE-BLOCK-TAG-IN  (apply alt (map string pre-block-tags))
                :LIST-BLOCK-TAG-IN (apply alt (map string list-block-tags))
                :INDENT            (hide-tag (apply cat (repeat indent (nt :SP))))}))

(defmacro with-delayed-body
  [[this] & body]
  `(let [~this (promise)] @(deliver ~this (do ~@body))))

(defn make-inline-parser
  []
  (comp concat-text
        (partial insta/transform transform-map)
        (insta/parser (merge rules {:BLOCK-TAG-IN      (alt (string "#"))
                                    :PRE-BLOCK-TAG-IN  (alt (string "%"))
                                    :LIST-BLOCK-TAG-IN (alt (string "*"))
                                    :INDENT            (hide-tag (cat (nt :SP) (nt :SP)))})
                      :start :TEXT-BODY)))

(defn make-parser
  [block-tags pre-block-tags list-block-tags indent]
  (let [r (make-rules block-tags pre-block-tags list-block-tags indent)]
    (with-delayed-body [this]
      (let [recur-this #(vector :BLOCK %1 (vec (mapcat splice (@this :BLOCKS %2))))]
        (fn [start-rule* text]
          (concat-text
            (insta/transform
              (merge transform-map {:BLOCK      recur-this
                                    :LIST-BLOCK recur-this
                                    :TEXT       (make-inline-parser)})
              ((insta/parser r :start start-rule*) text))))))))
