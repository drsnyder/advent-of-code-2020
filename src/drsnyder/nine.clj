(ns drsnyder.nine
  (:require [clojure.math.combinatorics :as combo]))

(defn lines->partitions [lines preamble]
  (map vec
       (partition (+ preamble 1) 1
                  (map #(Integer/parseInt %) lines))))

(defn find-first-failure [partitions]
  (loop [parts partitions]
    (let [block (first parts)
          n (last block)
          preamble (pop block)
          pairs (combo/combinations preamble 2)
          sums (set (map #(apply + %) pairs))]
      (if (not (sums n))
        n
        (recur (rest parts))))))

; part one 552655238
(defn part-one [lines preamble]
  (let [parts (lines->partitions lines preamble)]
    (find-first-failure parts)))
