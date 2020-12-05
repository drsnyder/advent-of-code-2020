(ns drsnyder.three)

(def clear \.)
(def tree \#)

(defn map-char [chr]
  (if (= chr clear)
    0
    1))

(defn line->vector [line width]
  (vec (flatten (repeat (+ width 1) (map map-char line)))))

(defn compute-width [data step-size]
  (let [data-len (count data)
        vector-len (count (first data))
        vector-steps (/ vector-len step-size)]
    (/ data-len vector-steps)))

(defn input->matrix [data step-size]
  (let [width (compute-width data step-size)]
    (vec (map #(line->vector % width) data))))

(defn steps->positions [height width step-right step-down]
  (let [down (range step-down height step-down)
        right (range step-right width step-right)]
    (prn (count down) (count right))
    (sort-by first (vec (zipmap down right)))))

(defn traverse-matrix [m step-right step-down]
  (let [pos (steps->positions (count m) (count (first m)) step-right step-down)]
    (reduce + (map #(get-in m %) pos))))

; first guess was 297 but that was too low
(defn traverse-hill-one [data step-right step-down]
  (let [m (input->matrix data step-right)]
    (traverse-matrix m step-right step-down)))

(defn traverse-hill-two [data config]
  (reduce *
          (map
            #(traverse-hill-one data (first %) (second %))
            config)))
