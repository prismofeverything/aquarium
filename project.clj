(defproject aquarium "0.0.1"
  :description "Clojurescript websockets project"
  :url "http://github.com/prismofeverything"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [http-kit "2.1.12"]
                 [polaris "0.0.4"]
                 [overtone/at-at "1.2.0"]
                 ;; [prismofeverything/bifocals "0.2.0"]
                 [clojurewerkz/neocons "3.0.0"]
                 [ring "1.2.0"]
                 [org.clojure/clojurescript "0.0-2156"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [domina "1.0.2"]
                 [com.keminglabs/singult "0.1.6"]
                 [org.clojure/google-closure-library-third-party "0.0-2029"]]
  :plugins [[lein-cljsbuild "1.0.2"]
            [com.keminglabs/cljx "0.4.0" 
             :exclusions [org.clojure/clojure]]]
  :source-paths ["src/clj" "target/generated/clj"]
  :resource-paths ["resources/"]
  :min-lein-version "2.0.0"
  :repl-options {:host "localhost"
                 :port 11911}
  :main aquarium.server
  :cljx 
  {:builds
   [{:source-paths ["src/cljx"]
     :output-path "target/generated/clj"
     :rules :clj}
    {:source-paths ["src/cljx"]
     :output-path "target/generated/cljs"
     :rules :cljs}]}
  :cljsbuild 
  {:builds 
   {:dev 
    {:libs ["singult"]
     :source-paths ["src/cljs" "target/generated/cljs"]
     :compiler 
     {:externs []
      :optimizations :whitespace
      :output-to  "resources/public/js/app/aquarium.js"
      :output-dir "resources/public/js/app/out"}}}})
