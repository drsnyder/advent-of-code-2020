(ns drsnyder.one)

(defn find-entries-set [report target-sum]
  (let [ss (into (sorted-set) report)]
    (first
      (filter #(not (nil? %))
              (map (fn [e]
                     (if-let [other (ss (- target-sum e))]
                       [e other]
                       nil))
                   ss)))))

(defn solve-first-report [report]
  (find-entries-set report 2020))

(defn solve-second-report [report]
  (first
    (filter #(not (nil? %))
            (map (fn [e]
                   (if-let [first-pair (find-entries-set report (- 2020 e))]
                     (conj first-pair e)))
                 (into (sorted-set) report)))))

(defn verify [pair]
  (let [total (apply + pair)
        product (apply * pair)]
    [total product]))
