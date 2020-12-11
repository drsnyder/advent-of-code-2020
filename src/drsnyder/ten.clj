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

(defn next-path-with-step [chain step]
  (let [looking-at (first chain)
        next-path (drop-while (fn [adapter]
                                (not (= (- adapter looking-at) step)))
                              chain)]
    (if (empty? next-path)
      nil
      [next-path step])))

(defn find-chains-dfs [chains prefix]
  (if (empty? chains)
    [prefix]
    (mapcat (fn [chain]
           (let [head (first chain)
                 next-paths (map first
                                 (filter #(not (nil? %))
                                         (map (partial next-path-with-step chain) [1 2 3])))]
             (find-chains-dfs next-paths (conj prefix head))))
         chains)))

(defn find-chains-iter [chain]
  (loop [chain-left chain
         list-of-branches []]
    (if (empty? chain-left)
      (pop list-of-branches)
      (let [next-paths (map second
                            (filter #(not (nil? %))
                               (map (partial next-path-with-step chain-left) [1 2 3])))
            max-step (if (empty? next-paths) 1 (apply max next-paths))]
        (recur (rest chain-left) (conj list-of-branches (count next-paths)))))))

; exhausts the heap
(defn part-two-dfs [lines]
  (let [[_ _ chain] (setup-chain lines)]
    (count (find-chains-dfs [chain] []))))

(defn part-two-iter [lines]
  (let [[_ _ chain] (setup-chain lines)
        results (find-chains-iter chain)]
    (prn results)
    (partition-by identity results)))
