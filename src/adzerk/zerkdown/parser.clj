(ns adzerk.zerkdown.parser
  (:refer-clojure :exclude [cat])
  (:require
    [clojure.java.io        :as io]
    [clojure.string         :as string]
    [instaparse.core        :as insta]
    [instaparse.combinators :as combi :refer [string cat alt neg star ebnf nt hide-tag]]))

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

(defn trans-inline
  [open & more]
  (let [[[data] close & chars] (reverse more)]
    [:INLINE [open close data] (concat-text (vec (reverse chars)))]))

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
   :INLINE                      trans-inline
   :PRE-INLINE                  trans-inline})

(let [counter (atom 0)]
  (defn make-inline-rules
    [{:keys [inline]}]
    (loop [rules {} [[start end] & xs] (seq inline) INLINE [] DELIM [(nt :ESC)]]
      (if (not start)
        (-> rules
            (assoc :INLINE       (apply alt INLINE)
                   :INLINE-DELIM (hide-tag (apply alt DELIM))))
        (let [n  (swap! counter inc)
              L1 (keyword (str "L" n))
              R1 (keyword (str "R" n))
              I1 (keyword (str "I" n))]
          (-> rules
              (assoc L1 (hide-tag (string start))
                     R1 (hide-tag (string end))
                     I1 (hide-tag (cat (nt L1)
                                       (star (alt (nt :INLINE) (nt :INLINE-CHAR)))
                                       (nt R1)
                                       (nt :INLINE-DATA))))
              (recur xs (conj INLINE (nt I1)) (into DELIM [(nt L1) (nt R1)]))))))))

(let [counter (atom 0)]
  (defn make-pre-inline-rules
    [{:keys [pre-inline]}]
    (loop [rules {} [[start end] & xs] (seq pre-inline) PRE-INLINE []]
      (if (not start)
        (assoc rules :PRE-INLINE (apply alt PRE-INLINE))
        (let [n   (swap! counter inc)
              PL1 (keyword (str "PL" n))
              PR1 (keyword (str "PR" n))
              PI1 (keyword (str "PI" n))]
          (-> rules
              (assoc PL1 (hide-tag (string start))
                     PR1 (hide-tag (string end))
                     PI1 (hide-tag (cat (nt PL1)
                                        (star (alt (cat (neg (alt (nt :ESC) (nt PR1))) (nt :ANY))
                                                   (nt :EOL)
                                                   (nt :ESCAPED)))
                                        (nt PR1)
                                        (nt :INLINE-DATA))))
              (recur xs (conj PRE-INLINE (nt PI1)))))))))

(defn make-rules
  [indent {:keys [block pre-block list] :as tags}]
  (->> {:BLOCK-TAG-IN      (apply alt (map string block))
        :PRE-BLOCK-TAG-IN  (apply alt (map string pre-block))
        :LIST-BLOCK-TAG-IN (apply alt (map string list))
        :INDENT            (hide-tag (apply cat (repeat indent (nt :SP))))}
       (merge rules (make-inline-rules tags) (make-pre-inline-rules tags))))

(defmacro with-delayed-body
  [[this] & body]
  `(let [~this (promise)] @(deliver ~this (do ~@body))))

(defn make-inline-parser
  [rules]
  (comp concat-text
        (partial insta/transform transform-map)
        (insta/parser rules :start :TEXT-BODY)
        (fn [& xs] (apply str xs))))

(defn parser
  [indent tags]
  (let [r (make-rules indent tags)]
    (with-delayed-body [this]
      (let [recur-this #(vector :BLOCK %1 (vec (mapcat splice (@this :BLOCKS %2))))]
        (fn parser*
          ([text]
           (parser* :BLOCKS text))
          ([start-rule* text]
             (concat-text
               (insta/transform
                 (merge transform-map {:BLOCK      recur-this
                                       :LIST-BLOCK recur-this
                                       :TEXT       (make-inline-parser r)})
                 ((insta/parser r :start start-rule*) text)))))))))
