(ns drsnyder.five)

(defn split-range [l u]
  (let [pivot (/ (- u l) 2)]
    [[l (+ pivot l)] [(+ pivot l 1) u]]))

(defn char->range [ranges inst]
  (if (or (= inst \B) (= inst \R))
    (map int (second ranges))
    (map int (first ranges))))

(defn split-code [code]
  (let [split (re-find #"([A-Z]{7})([A-Z]{3})" code)
        row-code (second split)
        seat-code (last split)]
    [row-code seat-code]))

(defn code->int [code l u]
  "(code-int FBFBBFF 0 127)"
  (let [instructions (map identity code)
        start-range (split-range l u)]
    (first (first
             (reduce (fn [ranges inst]
                       (let [new-inst (char->range ranges inst)]
                         (split-range (first new-inst) (second new-inst))))
                     start-range instructions)))))

(defn code->id [code]
  (let [[row-code seat-code] (split-code code)
        row-id (code->int row-code 0 127)
        seat-id (code->int seat-code 0 7)]
    (+ (* row-id 8) seat-id)))

(defn highest-id [lines]
  (let [ids (map code->id lines)]
    (last (sort ids))))

(defn find-missing-id [lines]
  (let [ids (sort (map code->id lines))
        adj (zipmap (conj ids 0) (concat ids (list (+ (last ids) 1))))]
    (second (filter (fn [p]
              (not (= (- (second p) (first p)) 1)))
            adj))))
