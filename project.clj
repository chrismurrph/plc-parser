(defproject plc-parser "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha13"]
                 [org.clojure/core.async "0.2.395"]
                 [com.taoensso/timbre "4.3.1"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [instaparse "1.4.2"]
                 ]
  :target-path "target/%s"

  :clean-targets ^{:protect false} ["target"]
  :source-paths ["dev/server"]

  :profiles {:dev {
                   :repl-options {
                                  :init-ns          user
                                  :port             7001
                                  }
                   :env          {:dev true}
                   :dependencies [[binaryage/devtools "0.5.2" :exclusions [environ]]
                                  [org.clojure/java.classpath "0.2.3"]
                                  [org.clojure/tools.namespace "0.2.11"]]}}
  )
