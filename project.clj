(defproject p-p-p-pokerface "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [iloveponies.tests/p-p-p-pokerface "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:dependencies [[midje "1.9.1"]]
                   :plugins [[lein-midje "3.2.1"]]}})
