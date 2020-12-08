(ns drsnyder.seven)

(defn parse-content-rule [rule]
  (if-let [parts (re-find #"^(\d+) (\w+ \w+) bags?$" rule)]
    {:num (second parts)
     :bag (nth parts 2)}))

(defn parse-bag-line [line]
  (re-find #"^(\w+ \w+) bags contain (:?(no other bags)|(\s?\d+ \w+ \w+ bags?,?)+)\.$"
           line))

(defn parse-bag-rule [line]
  "light red bags contain 1 bright white bag, 2 muted yellow bags."
  (let [parts (parse-bag-line line)
        bag (second parts)
        contents (clojure.string/split (nth parts 2) #", ")]
    {:bag bag
     :contents (filter #(not (nil? %))
                       (map parse-content-rule contents))}))


(defn rule->bags [rule]
  (map #(:bag %) (:contents rule)))

(defn can-carry? [bag rule]
  ((set (rule->bags rule)) bag))

(defn can-children-carry? [can-carry-set rule]
  (clojure.set/intersection can-carry-set (set (rule->bags rule))))

(defn lines->rules [lines]
  (map parse-bag-rule lines))

(defn find-direct-carry-bags [rules bag]
  (filter (partial can-carry? bag) rules))

(defn find-indirect-carry-bags [rules direct]
  (filter (partial can-children-carry? direct) rules))

(defn rule->bag-map [rules]
  (into (hash-map)
        (map (fn [rule]
               [(:bag rule) (set (rule->bags rule))])
             rules)))

(defn bag->rule-map [rules]
  (into (hash-map)
        (map (fn [rule]
               [(:bag rule) rule])
             rules)))

(defn fan-out [m bags]
  (flatten (filter #(not (nil? %))
                   (map seq (map #(get m %) bags)))))

(defn all-bags-in-tree [m bag]
  (loop [up-next (concat (fan-out m (get m bag)) (get m bag))
         all-bags up-next]
    (if (empty? up-next)
      all-bags
      (recur (concat (get m (first up-next))
                     (rest up-next)
                     (fan-out m (first up-next)))
             (set (concat up-next all-bags))))))

(defn rule-contents->bag-count [brm bag]
  (let [rule (get brm bag)
        contents (:contents rule)]
    (reduce + (map #(Integer/parseInt %) (map :num contents)))))



;;;;;;;;

; 600 is not right; subtract 1? try 599; 514?
(defn num-bags-in-tree-r [bmr bag]
  (let [children (:contents (get bmr bag))]
    (if (empty? children)
      0
      (reduce +
              (map (fn [child]
                     (let [num-child (Integer/parseInt (:num child))]
                       (+ num-child (* num-child (num-bags-in-tree-r bmr (:bag child))))))
                   children)))))


;;;;

; 118 too low, 254 just right
(defn part-one-bags [lines needle]
  "(part-one bags day7 \"shiny gold\")"
  (let [rules (lines->rules lines)
        m (rule->bag-map rules)
        child-sets (filter #(not (empty? %))
                           (map (partial all-bags-in-tree m) (keys m)))]
    (count (filter #(% needle) child-sets))))

; 568 is too low; 6006?
(defn part-two-total-bags [lines needle]
  (let [rules (lines->rules lines)
        m (rule->bag-map rules)
        brm (bag->rule-map rules)]
    (num-bags-in-tree-r brm needle)))
