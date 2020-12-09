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

(defn state [terminated loop-detected]
  (cond
    terminated :term
    loop-detected :loop
    :else nil))

(defn swap [inst]
  (condp = inst
    "nop" "jmp"
    "jmp" "nop"
    nil))

(defn find-to-replace [types insts]
  (loop [ptr 0
         to-replace []]
    (if (>= ptr (count insts))
      to-replace
      (let [{cur :inst arg :arg} (nth insts ptr)]
        (recur (+ ptr 1) (if (types cur)
                           (conj to-replace [ptr {:inst (swap cur) :arg arg}])
                           to-replace))))))

(defn run [insts]
  (loop [ptr 0
         acc 0
         visited #{}]
    (let [terminated (>= ptr (count insts))
          loop-detected (visited ptr)]
      (if (or terminated loop-detected)
        [acc (state terminated loop-detected)]
        (let [{cur :inst arg :arg} (nth insts ptr)
              [new-ptr new-acc] (exe-inst cur arg ptr acc)]
          (recur new-ptr new-acc (conj visited ptr)))))))

(defn part-one [lines]
  (let [insts (map line->inst lines)]
    (run insts)))

(defn part-two [lines]
  (let [insts (vec (map line->inst lines))
        to-replace (find-to-replace #{"nop" "jmp"} insts)]
    (loop [fix-attemps to-replace]
      (let [attempt (first fix-attemps)
            new-insts (assoc-in insts [(first attempt)] (second attempt))
            result (run new-insts)]
        (if (= :term (second result))
          (first result)
          (recur (rest fix-attemps)))))))
