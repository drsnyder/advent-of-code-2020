(ns user
  (:require [drsnyder.helpers :as h]
            [drsnyder.one :as one]
            [drsnyder.two :as two]
            [drsnyder.util.input :as input]))

(defn load-df [n]
  (clojure.string/split-lines (slurp (str "data/day-" n ".txt"))))

(def day1
  (map #(Integer/parseInt %)
       (clojure.string/split-lines (slurp "data/day-1.txt"))))

(def day2
  (clojure.string/split-lines (slurp "data/day-2.txt")))

(def day3
  (clojure.string/split-lines (slurp "data/day-3.txt")))

(comment
  (three/traverse-hill-one day3 3 1)
  (three/traverse-hill-two day3 [[1 1] [3 1] [5 1] [7 1] [1 2]]))

(h/start-nrepl-server!)
