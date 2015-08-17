(set-env!
  :resource-paths #{"src"}
  :source-paths   #{"test"}
  :dependencies   '[[org.clojure/clojure       "1.7.0"          :scope "provided"]
                    [tailrecursion/hoplon      "6.0.0-alpha5"   :scope "test"]
                    [tailrecursion/boot-hoplon "0.1.1"          :scope "test"]
                    [adzerk/bootlaces          "0.1.10"         :scope "test"]]
  :repositories   #(conj % '["sonatype" {:url "http://oss.sonatype.org/content/repositories/releases"}]))

(require
  '[clojure.java.io :as io]
  '[adzerk.bootlaces :refer :all])

(def +version+ "0.1.0-SNAPSHOT")

(bootlaces! +version+ :dev-dependencies "adzerk/zerkdown/pod_deps.edn")

(task-options!
  pom  {:project     'adzerk/zerkdown
        :version     +version+
        :description "Abstract markdownesque syntax to sexp parser."
        :url         "https://github.com/adzerk-oss/zerkdown"
        :scm         {:url "https://github.com/adzerk-oss/zerkdown"}
        :license     {"Eclipse Public License" "http://www.eclipse.org/legal/epl-v10.html"}})

