(defproject depsat "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.sat4j/org.sat4j.pb "[2.3.1,2.3.999)"]]
  :java-source-paths [ "java" ]
  :warn-on-reflection true
  :javac-options ["-target" "1.6" "-source" "1.6"]
  )
