(ns adzerk.zerkdown.parser-test
  (:require
    [clojure.java.io        :as io]
    [adzerk.zerkdown.parser :as parser :refer :all :reload true]
    [clojure.test           :as ctest  :refer [deftest is testing run-tests]]))

(def parser-of (partial make-parser ["#" "##"]))

(deftest parse-strings
  (let [parse  (parser-of :STRING)
        empty  "\"\""
        no-esc "\"asdf\""
        esc-dq "\"asdf\\\"qwer\""
        esc-cs "\"asdf\\tfoo\\\\qwer\""]
    (testing "empty string"
      (is (= empty (parse empty))))
    (testing "no escaped chars"
      (is (= no-esc (parse no-esc))))
    (testing "escaped double quotes"
      (is (= esc-dq (parse esc-dq))))
    (testing "other escaped chars"
      (is (= esc-cs (parse esc-cs))))))

(deftest parse-maps
  (let [parse   (parser-of :MAP)
        empty   "{}"
        simple  "{:foo 100}"
        map-str "{\"f{oo\" :bar}"
        nested  "{{:foo 100} \"200\"}"]
    (testing "empty map"
      (is (= empty (parse empty))))
    (testing "simple map"
      (is (= simple (parse simple))))
    (testing "containing strings with '{' in them"
      (is (= map-str (parse map-str))))
    (testing "containing nested maps"
      (is (= nested (parse nested))))))

(deftest parse-vecs
  (let [parse   (parser-of :VEC)
        empty   "[]"
        simple  "[:foo 100]"
        vec-str "[\"f[oo\" 100]"
        nested  "[[:foo 100] \"200\"]"]
    (testing "empty vec"
      (is (= empty (parse empty))))
    (testing "simple vec"
      (is (= simple (parse simple))))
    (testing "containing strings with '[' in them"
      (is (= vec-str (parse vec-str))))
    (testing "containing nested vecs"
      (is (= nested (parse nested))))))

(deftest clojure-coll
  (let [parse (parser-of :CLJ)
        coll  "[foo bar {:baz 200}]"]
    (testing "clojure collection"
      (is (= coll (parse coll))))))

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
      (is (= esc (parse esc))))))

(deftest indented-line
  (let [parse  (parser-of :INDENTED-LINE)
        empty  "\n"
        simple "  foo\n"]
    (testing "empty indented line"
      (is (= (list empty) (parse empty))))
    (testing "simple indented line"
      (is (= (list (subs simple 2)) (parse simple))))))

(deftest block
  (let [parse (parser-of :TAGGED-BLOCK)
        test1 "##[:foo 100] heyo  \n     qwer\n\n  foo\n  bar\n"
        test2 "## heyo  \n     qwer\n\n  foo\n  bar\n"
        test3 "##\n  heyo  \n     qwer\n\n  foo\n  bar\n"]
    (testing "parse block with clj"
      (is (= [:TAGGED-BLOCK
              ["##" "[:foo 100]"]
              "heyo  \n   qwer\n\nfoo\nbar\n"]
             (parse test1))))
    (testing ""
      (is (= [:TAGGED-BLOCK
              ["##"]
              "heyo  \n   qwer\n\nfoo\nbar\n"]
             (parse test2))))
    (testing ""
      (is (= [:TAGGED-BLOCK
              ["##"]
              "heyo  \n   qwer\n\nfoo\nbar\n"]
             (parse test3))))))

(deftest blocks
  (let [parse (parser-of :BLOCKS)]
    (testing "blockes, tagged and text"
      (is (= [[:TAGGED-BLOCK
               ["#" "[foo]"]
               "asdf\nqwer asdf zxcv\nfoop de doop\n\n"]
              "this is a block\nof plain text\nyou see\n"
              [:TAGGED-BLOCK ["##" "[bar]"]
               "some more\ntagged blocks\n"]
              "and finally the\nend of the text\n"]
             (parse (slurp (io/resource "adzerk/zerkdown/parser_test/block1.zd"))))))))
