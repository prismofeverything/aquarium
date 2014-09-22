(ns aquarium.neo
  (:require 
   [clojurewerkz.neocons.rest :as neo]
   [clojurewerkz.neocons.rest.nodes :as node]
   [clojurewerkz.neocons.rest.relationships :as edge]))

(def connection
  (neo/connect "http://localhost:7474/db/data"))

