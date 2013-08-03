(defproject slimath "0.2.1"
  :description "Simple math library"
  :url "http://sliplanesoftware.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.numeric-tower "0.0.2"]]
  :profiles {:dev {:dependencies [[midje "1.5.0"]]}})
