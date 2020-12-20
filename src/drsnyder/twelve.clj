(ns drsnyder.twelve)

(defn line->inst [line]
  (let [parts (re-find #"([A-Z]{1})(\d+)" line)]
    {:inst (second parts) :units (Integer/parseInt (last parts))}))

(def start-state {
                   :x 0
                   :y 0
                   :facing :east
                   })

(def dir-map {       ; 90 180 270 360
              :north [:east :south :west]
              :south [:west :north :east]
              :east [:south :west :north]
              :west [:north :east :south]
              })

(defn rotate-to [from direction units]
  (let [cards (if (= direction :right) (from dir-map) (vec (reverse (from dir-map))))
        idx (- (/ units 90) 1)]
    (nth cards idx)))



(defn change-direction [state facing direction units]
  (assoc-in state [:facing] (rotate-to facing direction units)))

(defn update-state-coord [state direction units]
  (condp = direction
    :east (update-in state [:x] + units)
    :west (update-in state [:x] - units)
    :north (update-in state [:y] + units)
    :south (update-in state [:y] - units)
    state))

(defn move [state inst units]
  (let [facing (:facing state)]
    (condp = inst
      "F" (update-state-coord state facing units)
      "N" (update-state-coord state :north units)
      "S" (update-state-coord state :south units)
      "E" (update-state-coord state :east units)
      "W" (update-state-coord state :west units)
      "R" (change-direction state facing :right units)
      "L" (change-direction state facing :left units)
      state)))

(defn part-one [lines]
  (let [insts (map line->inst lines)]
    (loop [cmds insts
           state start-state]
      (if (empty? cmds)
        (let [{x :x y :y} state]
          [(+ (Math/abs x) (Math/abs y)) state])
        (let [{inst :inst units :units} (first cmds)]
          (recur (rest cmds) (move state inst units)))
        ))))

(def start-state-2 {
                     :ship {:x 0 :y 0}
                     :waypoint {:x 10 :y 1}
                     })

(defn rotate-waypoint [state direction degrees]
  (if (= 0 degrees)
    state
    (let [waypoint (:waypoint state)]
      (condp = direction
      "L" (recur (assoc-in state [:waypoint] {:x (* -1 (:y waypoint)) :y (:x waypoint)})
                 direction
                 (- degrees 90))
      "R" (recur (assoc-in state [:waypoint] {:x (:y waypoint) :y (* -1 (:x waypoint))})
                 direction
                 (- degrees 90))
      state))))

(defn move-ship->waypoint [state units]
  (let [ship (:ship state)
        wp (:waypoint state)
        move-x (+ (:x ship) (* units (:x wp)))
        move-y (+ (:y ship) (* units (:y wp)))]
    (merge state {:ship {:x move-x :y move-y}})))

(defn move-waypoint [state direction units]
  (let [](condp = direction
    "N" (update-in state [:waypoint :y] + units)
    "S" (update-in state [:waypoint :y] - units)
    "E" (update-in state [:waypoint :x] + units)
    "W" (update-in state [:waypoint :x] - units)
    )))

(defn part-two [lines]
  (let [insts (map line->inst lines)]
    (loop [cmds insts
           state start-state-2]
      (if (empty? cmds)
        (let [{{x :x y :y} :ship} state]
          [(+ (Math/abs x) (Math/abs y)) state])
        (let [{inst :inst units :units} (first cmds)]
          (cond
            (#{"L" "R"} inst) (recur (rest cmds) (rotate-waypoint state inst units))
            (= "F" inst) (recur (rest cmds) (move-ship->waypoint state units))
            :else (recur (rest cmds) (move-waypoint state inst units))))))))

