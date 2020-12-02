(ns user
  (:require [drsnyder.helpers :as h]
            [drsnyder.one :as one]))

(def day1
  (map #(Integer/parseInt %)
       (clojure.string/split-lines (slurp "data/day-1.txt"))))


(h/start-nrepl-server!)
