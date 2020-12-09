(ns drsnyder.eight)

(defn line->inst [line]
  (let [parts (re-find #"^([a-z]{3}) ([-+]{1}\d+)$" line)]
    {:inst (second parts) :arg (Integer/parseInt (last parts))}))

(defn exe-inst [inst arg ptr acc]
  (condp = inst
    "nop" [(+ ptr 1) acc]
    "jmp" [(+ ptr arg) acc]
    "acc" [(+ ptr 1) (+ acc arg)]
    [nil nil]))

(defn run [insts]
  (loop [ptr 0
         acc 0
         visited #{}]
    (if (or (>= ptr (count insts)) (visited ptr))
      acc
      (let [{cur :inst arg :arg} (nth insts ptr)
            [new-ptr new-acc] (exe-inst cur arg ptr acc)]
        (recur new-ptr new-acc (conj visited ptr))))))

(defn part-one [lines]
  (let [insts (map line->inst lines)]
    (run insts)))
