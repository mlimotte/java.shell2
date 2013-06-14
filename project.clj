(defproject com.climate/java.shell2 "0.1.0"
  :description "Functions for launching processes with shell-like redirection"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths ["src/main/clojure"]
  :test-paths   ["src/test/clojure"]

  :dependencies [[org.clojure/clojure "1.5.1"]]

  :profiles {:1.2 {:dependencies [[org.clojure/clojure "1.2.1"]]}
             :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}}

  :aliases {"test-all" ["with-profile" "1.2:1.3:1.4:1.5" "test"]}

  )
