(defproject gentone "0.1.0-SNAPSHOT"
  :description "Generate musical sequences using genetic programming."
  :url "http://example.com/FIXME"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [kunstmusik/pink "0.3.0"]
                 [fungp "0.3.2"]
                 [cfft "0.1.0"]]
  :profiles {:user {:dependencies [[org.clojure/tools.namespace "0.2.11"]]}})
