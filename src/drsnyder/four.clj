(ns drsnyder.four
  (:require [drsnyder.util.input :refer [lines->records]]))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"})

(defn validate-year [mn mx y]
  (if (re-matches #"\d{4}" y)
    (let [py (Integer/parseInt y)]
      (>= mx py mn))
    false))

(def validators {
                 "byr" (partial validate-year 1920 2002)
                 "iyr" (partial validate-year 2010 2020)
                 "eyr" (partial validate-year 2020 2030)
                 "hgt" (fn [h]
                         (if-let [v (re-find #"^(\d{2,3})(cm|in)$" h)]
                           (if (= (last v) "cm")
                             (>= 193 (Integer/parseInt (second v)) 150)
                             (>= 76 (Integer/parseInt (second v)) 59))
                           false))
                 "hcl" #(re-matches #"^#[0-9a-f]{6}$" %)
                 "ecl" #(re-matches #"^(amb|blu|brn|gry|grn|hzl|oth)$" %)
                 "pid" #(re-matches #"^\d{9}$" %)
                 "cid" (fn [_] true)
                 })

(defn passport->map [pp]
  (into (hash-map)
        (map #(clojure.string/split % #":")
             (clojure.string/split pp #" "))))

(defn valid-passport-fields? [pp]
  (let [ks (set (keys (passport->map pp)))
        diff (clojure.set/difference required-fields ks)]
    (or (= (count diff) 0) (= diff #{"cid"}))))

; 208
(defn part-one-count [lines]
  (let [pp (lines->records lines)]
    (count (filter true? (map valid-passport-fields? pp)))))

(defn valid-passport-values? [pp]
  (if (valid-passport-fields? pp)
    (let [m (passport->map pp)]
      (every? identity
             (map #((validators %) (m %)) (keys m))))
    false))

; 167
; this isn't the most efficient. we should move the max extraction out of the
; validation methods
(defn part-two-count [lines]
  (let [pp (lines->records lines)]
    (count (filter valid-passport-values? (filter valid-passport-fields? pp)))))
