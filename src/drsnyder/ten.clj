(ns drsnyder.ten)

(defn lines->jolts [lines]
  (sort (map #(Integer/parseInt %) lines)))

(defn setup-chain [lines]
  (let [adapters (lines->jolts lines)
        device (+ 3 (apply max adapters))
        chain (concat (list 0) adapters (list device))]
    [adapters device chain]))

; 2450
(defn part-one [lines]
  (let [[adapters device chain] (setup-chain lines)
        adjacent (partition 2 1 chain)]
    (reduce * (vals (frequencies (map #(apply - (reverse %)) adjacent))))))

(defn next-path-of-length [looking-at chain step]
  (if (= (- (first chain) looking-at) step)
    (rest chain)
    nil))

(defn next-path-with-step [chain step]
  (let [looking-at (first chain)
        next-path (drop-while (fn [adapter]
                                (not (= (- adapter looking-at) step)))
                              chain)]
    (if (empty? next-path)
      nil
      next-path)))

(defn find-next-paths-for-all-chains [chains steps]
  (mapcat (fn [chain]
               (filter #(not (nil? %))
                       (map (partial next-path-with-step chain) steps)))
             chains))

;(defn find-chains [lines jolts]
  ;(let [[adapters device chain] (setup-chain lines)]
    ;(loop [head (list (first chain))
           ;paths (set (list chain))
           ;acc [chain]]

      ;(if (empty? paths)
        ;acc
        ;(let [new-paths (map #(concat head %)
                             ;(find-next-paths-for-all-chains (seq paths) jolts))]
          ;(prn [head (clojure.set/union paths (set new-paths)) acc])
          ;(recur (concat head ()) nil [])))
    ;)))

(defn find-chains [chains]
  (if (empty? chains)
    nil
    (for [chain chains]
      (let [head (list (first chain))
            next-paths (filter #(not (nil? %))
                               (map (partial next-path-with-step chain) [1 2 3]))]
        (find-chains next-paths)))))
