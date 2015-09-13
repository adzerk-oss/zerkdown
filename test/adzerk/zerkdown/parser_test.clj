(ns adzerk.zerkdown.parser-test
  (:require
    [clojure.java.io        :as io]
    [clojure.pprint         :as pp     :refer :all]
    [adzerk.zerkdown.parser :as parser :refer :all :reload true]
    [clojure.test           :as ctest  :refer [deftest is testing run-tests]]))

(def BLOCK-TAGS      ["#" "##"])
(def PRE-BLOCK-TAGS  ["%" "%%"])
(def LIST-BLOCK-TAGS ["*" "**"])
(def INDENT          2)

(def the-parser (make-parser BLOCK-TAGS PRE-BLOCK-TAGS LIST-BLOCK-TAGS INDENT))

(defn parser-of [x] (make-parser BLOCK-TAGS PRE-BLOCK-TAGS LIST-BLOCK-TAGS INDENT x))
(defn failure?  [x] (= instaparse.gll.Failure (type x)))
(defn resource  [x] (->> x (str "adzerk/zerkdown/parser_test/") io/resource slurp))

(deftest parse-strings
  (let [parse  (parser-of :STRING)
        empty  "\"\""
        no-esc "\"asdf\""
        esc-dq "\"asdf\\\"qwer\""
        esc-cs "\"asdf\\tfoo\\\\qwer\""
        fail1  "\"asdf\\\""]
    (testing "empty string"
      (is (= empty (parse empty))))
    (testing "no escaped chars"
      (is (= no-esc (parse no-esc))))
    (testing "escaped double quotes"
      (is (= esc-dq (parse esc-dq))))
    (testing "other escaped chars"
      (is (= esc-cs (parse esc-cs))))
    (testing "unbalanced escaped double quote"
      (is (failure? (parse fail1))))))

(deftest parse-maps
  (let [parse   (parser-of :MAP)
        empty   "{}"
        simple  "{:foo 100}"
        map-str "{\"f{oo\" :bar}"
        nested  "{{:foo 100} \"200\"}"
        fail1   "{{:foo 100} \"200}\""]
    (testing "empty map"
      (is (= empty (parse empty))))
    (testing "simple map"
      (is (= simple (parse simple))))
    (testing "containing strings with '{' in them"
      (is (= map-str (parse map-str))))
    (testing "containing nested maps"
      (is (= nested (parse nested))))
    (testing "unbalanced curlies with curly in string"
      (is (failure? (parse fail1))))))

(deftest parse-vecs
  (let [parse   (parser-of :VEC)
        empty   "[]"
        simple  "[:foo 100]"
        vec-str "[\"f[oo\" 100]"
        nested  "[[:foo 100] \"200\"]"
        nonsym  "[this#isn't^a/valid.clojure\\symbol]"
        escaped "[\\}]"
        fail1   "[}]"]
    (testing "empty vec"
      (is (= empty (parse empty))))
    (testing "simple vec"
      (is (= simple (parse simple))))
    (testing "containing strings with '[' in them"
      (is (= vec-str (parse vec-str))))
    (testing "containing nested vecs"
      (is (= nested (parse nested))))
    (testing "containing atom that isn't parseable as clojure data"
      (is (= nonsym (parse nonsym))))
    (testing "escaped unbalanced curly in vec"
      (is (= escaped (parse escaped))))
    (testing "unbalanced curlies in vec"
      (is (failure? (parse fail1))))))

(deftest clojure-coll
  (let [parse (parser-of :DATA)
        coll  "[foo (bar {:baz 200})]"
        fail1 "[foo ] bar]"
        fail2 "\\[foo bar]"]
    (testing "valid data"
      (is (= coll (parse coll))))
    (testing "invalid data"
      (is (failure? (parse fail1))))
    (testing "escaped open brace"
      (is (failure? (parse fail2))))))

(deftest line
  (let [parse  (parser-of :LINE)
        empty  "\n"
        simple "fo]op\n"
        esc    "fo]o\\nbar\n"]
    (testing "empty line"
      (is (= empty (parse empty))))
    (testing "simple line"
      (is (= simple (parse simple))))
    (testing "escaped newline"
      (is (= "fo]onbar\n" (parse esc))))))

(deftest indented-line
  (let [parse  (parser-of :INDENTED-LINE)
        empty  "\n"
        simple "  foo\n"]
    (testing "empty indented line"
      (is (= (list empty) (parse empty))))
    (testing "simple indented line"
      (is (= (list (subs simple 2)) (parse simple))))))

(deftest blocks
  (let [parse (parser-of :BLOCKS)]
    (testing "blockes, tagged and text"
      (is (= [[:BLOCK
               ["#" "[foo]"]
               ["asdf qwer asdf\nzxcv foop de doop\n"]]
              "this is a block of\nplain text, you see\n"
              [:BLOCK
               ["##" "[bar]"]
               ["a one liner\n"]]
              "and finally some\nmore plain text...\n# and this is still\nregular text because\nthe hash was escaped.\n"]
             (parse (resource "block1.zd")))))
    (testing "nested blocks"
      (is (= [[:BLOCK
               ["#"]
               ["This Is A Block\n"
                [:BLOCK
                 ["#"]
                 ["That contains\na nested block,\n"]]
                "and then contin-\nues like this for\na while...\n"]]
              "and finally ends\nwith a block of\nnormal, regular text.\n"]
             (parse (resource "block2.zd")))))))

(deftest list-blocks
  (let [parse (parser-of :BLOCKS)]
    (testing "list blocks"
      (is (= [[:BLOCK
               ["#"]
               ["Foo Bar\n"]]
              "This is some\ntext to see if\nthe damn thing\neven works...\n"
              [:LIST
               ["*"]
               [[:BLOCK
                 ["*" "[omfg]"]
                 ["an unordered\nqith foo bar\n"
                  [:BLOCK
                   ["#"]
                   ["a block nested \nin the list item\n"]]]]
                [:BLOCK
                 ["*" "[hey there]"]
                 ["list with\n"]]
                [:BLOCK
                 ["*"]
                 ["three items\n"]]]]
              [:LIST
               ["**"]
               [[:BLOCK
                 ["**"]
                 ["asdf qwer\n"]]
                [:BLOCK
                 ["**"]
                 ["zxcv ;lkj\n"]]]]
              "And here is\nthe end of the\ntext, fuckit.\n"]
             (parse (resource "block4.zd")))))))

(the-parser :BLOCKS "# asdf\n  hi there <<![foo x[bar\\]](omg) ^[baz baf]^ qwerqwer>>\n\n")
((make-inline-parser) "asdf ![foo^[bar]](omg) qwerqwer\n\n")


[[:BLOCK
  ["#"]
  ["asdf\nhi there "
   [:INLINE
    ["![" "]" "(omg)"]
    ["foo "
     [:INLINE
      ["^[" "]" nil]
      ["bar"]]]]
   " qwerqwer\n"]]]

