(ns user
  (:require [clojure.java.javadoc :refer [javadoc]]
            [clojure.pprint :refer [pprint]]
            [clojure.reflect :refer [reflect]]
            [clojure.repl :refer [apropos dir doc find-doc pst source]]
            [clojure.tools.namespace.repl :only [refresh refresh-all clear] :refer :all]
            [gentone.core :refer :all]
            [gentone.math :refer :all]
            [gentone.pitch :refer :all]
            [gentone.time :refer :all]
            [gentone.sequencer :refer :all]))
