(ns drsnyder.seven)

(defn parse-content-rule [rule]
  (let [parts (re-find #"^(\d+) (\w+ \w+) bags?$" rule)]
    {:num (second parts)
     :bag (nth parts 2)}))

; ^([\w\space]+) bags contain ((no other bags)|(\d+ \w+ \w+ bags?)+(, (\d+ \w+ \w+ bags?))+)\.$

(defn parse-bag-line [line]
  (re-find #"^([\w\space]+) bags contain ((no other bags)|(\d+ \w+ \w+ bags?)+(:?, (\d+ \w+ \w+ bags?))*)\.$"
           line))

(defn parse-bag-rule [line]
  "light red bags contain 1 bright white bag, 2 muted yellow bags."
  (let [parts (parse-bag-line line)
        bag (second parts)
        contents (filter #(not (nil? %)) (take-nth 2 (drop 4 parts)))]
    {:bag bag
     :contents (map parse-content-rule contents)}))
