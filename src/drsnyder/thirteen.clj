(ns drsnyder.thirteen)

(defn parse [lines]
  (let [first-depart (Integer/parseInt (first lines))
        buses (clojure.string/split (second lines) #",")]
    [first-depart
     (map (fn [bus]
            (if (= "x" bus)
              bus
              (Integer/parseInt bus)))
          buses)]))

(defn can-depart [ts ids]
  (first
    (filter #(= 0 (rem ts %))
            (filter #(not= "x" %) ids))))

(defn part-one [lines]
  (let [[ts ids] (parse lines)]
    (loop [now ts]
      (if-let [bus (can-depart now ids)]
        (* (- now ts) bus)
        (recur (inc now))))))
