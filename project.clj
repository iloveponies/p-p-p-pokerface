(defproject p-p-p-pokerface "1.0.0-SNAPSHOT"
  :description "P-p-p-pokerface"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :repositories {"stuart" "http://stuartsierra.com/maven2"}
  :plugins [[lein-midje "2.0.0-SNAPSHOT"]]
  :profiles
    {:dev
     {:dependencies [[midje "1.4.0"]
                     [com.stuartsierra/lazytest "1.2.3"]]}})
