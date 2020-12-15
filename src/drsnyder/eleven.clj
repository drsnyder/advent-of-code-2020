(ns drsnyder.eleven)

(def floor \.)
(def empty-seat \L)
(def occupied-seat \#)

(defn line->vector [line]
  (vec (map identity line)))

(defn lines->waiting-area [lines]
  (vec (map line->vector lines)))

(defn dims [v]
  (list (count v) (count (first v))))

(defn seat? [s]
  (#{empty-seat occupied-seat} s))

(defn first-fn [v x y dir-fn x-range y-range]
  (first (filter #(or (seat? %) (nil? %))
                 (map #(apply (partial dir-fn v) %)
                      ; this pairs the ranges together
                      (map vector x-range y-range)))))

(defn diag-up-left [v x y]
  (get-in v [(- y 1) (- x 1)]))

(defn first-diag-up-left [v x y]
  (first-fn v x y diag-up-left (range x -1 -1) (range y -1 -1)))

(defn up [v x y]
  (get-in v [(- y 1) x]))

(defn first-up [v x y]
  (first-fn v x y up (cycle [x]) (range y -1 -1)))

(defn diag-up-right [v x y]
  (get-in v [(- y 1) (+ x 1)]))

(defn first-diag-up-right [v x y]
  (let [[h w] (dims v)]
    (first-fn v x y diag-up-right (range x w) (range y -1 -1))))

(defn left [v x y]
  (get-in v [y (- x 1)]))

(defn first-left [v x y]
  (first-fn v x y left (range x -1 -1) (cycle [y])) )

(defn right [v x y]
  (get-in v [y (+ x 1)]))

(defn first-right [v x y]
  (let [[h w] (dims v)]
    (first-fn v x y right (range x w) (cycle [y]))))

(defn diag-down-left [v x y]
  (get-in v [(+ y 1) (- x 1)]))

(defn first-diag-down-left [v x y]
  (let [[h w] (dims v)]
   (first-fn v x y diag-down-left (range x -1 -1) (range y h))))

(defn down [v x y]
  (get-in v [(+ y 1) x]))

(defn first-down [v x y]
  (let [[h w] (dims v)]
   (first-fn v x y down (cycle [x]) (range y h))))

(defn diag-down-right [v x y]
  (get-in v [(+ y 1) (+ x 1)]))

(defn first-diag-down-right [v x y]
  (let [[h w] (dims v)]
   (first-fn v x y diag-down-right (range x w) (range y h))))

(defn seat-state [v x y]
  (get-in v [y x]))

(defn update-seat-state [v x y new-state]
  (assoc-in v [y x] new-state))

(defn adjacent-seats [v x y]
  [(diag-up-left v x y)
   (up v x y)
   (diag-up-right v x y)
   (left v x y)
   (right v x y)
   (diag-down-left v x y)
   (down v x y)
   (diag-down-right v x y)])

(defn first-adjacent-seats [v x y]
  [(first-diag-up-left v x y)
   (first-up v x y)
   (first-diag-up-right v x y)
   (first-left v x y)
   (first-right v x y)
   (first-diag-down-left v x y)
   (first-down v x y)
   (first-diag-down-right v x y)])

(defn no-adj-occupied? [adjacent]
  (not-any? #(= occupied-seat %) adjacent))

(defn should-become-empty? [adjacent n]
  (>= (count (filter #(= occupied-seat %) adjacent)) n))

(defn change-state-1 [original v coord]
  (let [[x y] coord
        adjacent (adjacent-seats original x y)]
    (condp = (seat-state original x y)
      empty-seat (if (no-adj-occupied? adjacent)
                   (update-seat-state v x y occupied-seat)
                   v)
      occupied-seat (if (should-become-empty? adjacent 4)
                      (update-seat-state v x y empty-seat)
                      v)
      v)))

(defn change-state-2 [original v coord]
  (let [[x y] coord
        adjacent (first-adjacent-seats original x y)]
    (condp = (seat-state original x y)
      empty-seat (if (no-adj-occupied? adjacent)
                   (update-seat-state v x y occupied-seat)
                   v)
      occupied-seat (if (should-become-empty? adjacent 5)
                      (update-seat-state v x y empty-seat)
                      v)
      v)))

(defn visit-all-seats [v state-fn]
  (let [[h w] (dims v)
        positions (for [x (range 0 w)
                        y (range 0 h)] [x y])]
    (reduce (partial state-fn v) v positions)))

(defn num-occupied [v]
  (count (filter #(= % occupied-seat) (flatten v))))

(defn part-one [lines]
  (let [v (lines->waiting-area lines)]
    (loop [current v
           prior nil
           iter 0]
      (if (= prior current)
        [(num-occupied current) iter]
        (recur (visit-all-seats current change-state-1) current (+ iter 1))))))

; not quite right
(defn part-two [lines]
  (let [v (lines->waiting-area lines)]
    (loop [current v
           prior nil
           iter 0]
      (if (= prior current)
        [(num-occupied current) iter]
        (recur (visit-all-seats current change-state-2) current (+ iter 1))))))
