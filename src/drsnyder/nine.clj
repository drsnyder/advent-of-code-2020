(ns drsnyder.nine
  (:require [clojure.math.combinatorics :as combo]))

(defn lines->partitions [lines preamble]
  (map vec
       (partition preamble 1
                  (map #(bigint %) lines))))

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
  (let [parts (lines->partitions lines (+ preamble 1))]
    (find-first-failure parts)))

(defn parts->seq-sums [parts]
  (map (fn [part]
         [(apply + part) part])
       parts))

(defn find-failure-sequence-by-preamble [lines preamble sum-target]
  (let [parts (lines->partitions lines preamble)
        sum-seqs (parts->seq-sums parts)]
    (first (filter #(= (first %) sum-target) sum-seqs))))

; 66770727 is too low; 70672245 is just right
(defn part-two [lines preamble]
  (let [target-sum (part-one lines preamble)]
    (loop [seq-size 2]
      (let [found (second (find-failure-sequence-by-preamble lines seq-size target-sum))]
        (if found
          [target-sum (+ (apply min found) (apply max found))]
          (recur (+ seq-size 1)))))))
