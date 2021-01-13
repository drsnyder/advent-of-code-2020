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

(defn exp [x n]
  (if (= n 0)
    x
    (let [x (bigint x)]
      (loop [acc x
             n n]
        (if (= n 1)
          acc
          (recur (* acc x) (- n 1)))))))

(defn exp-stream
  ([x] (exp-stream x 1))
  ([x n] (cons (exp x n) (lazy-seq (exp-stream x (inc n))))))

(defn mod-stream
  ([x] (mod-stream (bigint x) 1))
  ([x n] (lazy-seq (cons (+ x (* x n)) (mod-stream x (inc n))))))

(defn aligned-buses [ids ts]
  (if (empty? ids)
    true
    (if (= "x" (first ids))
      (aligned-buses (rest ids) (inc ts))
      (if (= (rem ts (first ids)) 0)
        (aligned-buses (rest ids) (inc ts))
        false))))

(defn find-first-alignment [ids offset]
  (let [x (first ids)
        start-times (mod-stream x offset)]
    (first (filter (partial aligned-buses ids) start-times))))

(defn find-matching-offset [ts offset-ts bus skip]
  (loop [min-to-depart (+ ts offset-ts)
         cur-ts ts]
    (if (= (rem min-to-depart bus) 0)
      cur-ts
      (let [cur-ts (+ cur-ts skip)]
       (recur (+ cur-ts offset-ts) cur-ts)))))

(defn part-two [lines]
  (let [buses (second (parse lines))
        indexed (vec (zipmap (range (count buses)) buses))
        indexed (filter #(not= "x" (second %)) indexed)]
    (loop [buses indexed
           bus (second (first buses))
           offset-ts (first (first buses))
           acc [bus]
           skip (apply * acc)
           ts 0]
      (if (empty? buses)
        ts
        (let [nxt-ts (find-matching-offset ts offset-ts bus skip)
              nxt-buses (rest buses)
              nxt-bus (second (first nxt-buses))
              nxt-offset-ts (first (first nxt-buses))
              nxt-acc (conj acc nxt-bus)
              nxt-skip (apply * acc)]
          (recur nxt-buses nxt-bus nxt-offset-ts nxt-acc nxt-skip nxt-ts))))))
