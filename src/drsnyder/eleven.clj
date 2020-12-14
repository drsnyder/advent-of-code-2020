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

(defn diag-up-left [v x y]
  (get-in v [(- y 1) (- x 1)]))

(defn up [v x y]
  (get-in v [(- y 1) x]))

(defn diag-up-right [v x y]
  (get-in v [(- y 1) (+ x 1)]))

(defn left [v x y]
  (get-in v [y (- x 1)]))

(defn right [v x y]
  (get-in v [y (+ x 1)]))

(defn diag-down-left [v x y]
  (get-in v [(+ y 1) (- x 1)]))

(defn down [v x y]
  (get-in v [(+ y 1) x]))

(defn diag-down-right [v x y]
  (get-in v [(+ y 1) (+ x 1)]))

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

(defn no-adj-occupied? [adjacent]
  (not-any? #(= occupied-seat %) adjacent))

(defn should-become-empty? [adjacent]
  (>= (count (filter #(= occupied-seat %) adjacent)) 4))

(defn change-state [original v coord]
  (let [[x y] coord
        adjacent (adjacent-seats original x y)]
    (condp = (seat-state original x y)
      empty-seat (if (no-adj-occupied? adjacent)
                   (update-seat-state v x y occupied-seat)
                   v)
      occupied-seat (if (should-become-empty? adjacent)
                      (update-seat-state v x y empty-seat)
                      v)
      v)))

(defn visit-all-seats [v]
  (let [[h w] (dims v)
        positions (for [x (range 0 w)
                        y (range 0 h)] [x y])]
    (reduce (partial change-state v) v positions)))

(defn num-occupied [v]
  (count (filter #(= % occupied-seat) (flatten v))))

(defn part-one [lines]
  (let [v (lines->waiting-area lines)]
    (loop [current v
           prior nil
           iter 0]
      (if (= prior current)
        [(num-occupied current) iter]
        (recur (visit-all-seats current) current (+ iter 1))))))
