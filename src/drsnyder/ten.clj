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

(defn get-start-pos [chain n]
  (.indexOf chain n))

; exhausts the heap
(defn part-two-dfs [lines]
  (let [[_ _ chain] (setup-chain lines)]
    (count (find-chains-dfs [chain] []))))

(defn part-two-iter [lines]
  (let [[_ _ chain] (setup-chain lines)
        results (find-chains-iter chain)]
    (prn results)
    (partition-by identity results)))

(defn split-paths [paths chain]
  (map first
       (filter #(not (nil? %))
               (mapcat (fn [p]
                         (let [s (drop (get-start-pos chain p) chain)]
                           (map (comp first (partial next-path-with-step s)) [1 2 3])))
                       paths))))
(def split-paths-m (memoize split-paths))

; OOM heap space
(defn part-two-iter-2 [lines]
  (let [[_ device chain] (setup-chain lines)]
    (loop [paths [(first chain)]
           finished 0]
      (if (empty? paths)
        finished
        (let [new-pos (split-paths-m paths chain)
              new-finished (+ finished (count (filter #(= % device) new-pos)))
              next-round (filter #(not= % device) new-pos)]
          (recur next-round new-finished))))))

(defn get-vec-pos [v pos]
  (if (< pos 0)
    0
    (get v pos)))


; 32396521357312
; What an elegant solution. I had to get some help from reddit for this one.
; https://www.reddit.com/r/adventofcode/comments/ka8z8x/2020_day_10_solutions/gfnsrjv/?utm_source=reddit&utm_medium=web2x&context=3
; https://hackernoon.com/google-interview-questions-deconstructed-the-knights-dialer-f780d516f029
(defn part-two-iter-3 [lines]
  (let [[_ _ chain] (setup-chain lines)
        counts (vec (concat (list 1) (take (last chain) (cycle (list 0)))))]
    (last (reduce (fn [v pos]
                    (prn v)
                    (assoc v pos (+ (get-vec-pos v (- pos 3))
                                    (get-vec-pos v (- pos 2))
                                    (get-vec-pos v (- pos 1)))))
                  counts
                  (rest chain)))))
