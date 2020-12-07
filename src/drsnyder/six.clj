(ns drsnyder.six
  (:require [drsnyder.util.input :refer [lines->records]]))

(defn record->all-answers [record]
  (set (filter (partial not= \space) (map identity record))))

(defn part-one-counts [lines]
  (let [records (lines->records lines)]
    (reduce + (map (comp count record->all-answers) records))))

(defn record->each-answer [record]
  (reduce clojure.set/intersection
          (map (comp set identity)
               (re-seq #"[a-z]+" record))))

(defn part-two-counts [lines]
  (let [records (lines->records lines)]
    (reduce + (map (comp count record->each-answer) records))))
