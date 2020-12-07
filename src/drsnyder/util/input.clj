(ns drsnyder.util.input)


(defn lines->records [lines]
  (map (partial clojure.string/join " ")
       (filter #(not (= % (list "")))
               (partition-by #(= % "") lines))))
