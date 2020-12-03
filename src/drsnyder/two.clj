(ns drsnyder.two)

(defn line->rule+password [line]
  (let [[rule pw] (clojure.string/split line #": ")]
    {:rule rule :pw pw}))

(defn parse-rule->map [rule]
  (let [[mn mx ch] (clojure.string/split rule #"[- ]")]
    {:min (Integer/parseInt mn) :max (Integer/parseInt mx) :char (first ch)}))

(defn password->char-count [pw chr]
  (count (filter #(= % chr) (map identity pw))))

(defn password->char-positions [pw chr]
  (into (sorted-set)
        (map first
             (filter #(= (second %) chr)
                     (vec (zipmap (range 1 (+ (count pw) 1)) (map identity pw)))))))

(defn valid-password-part-one? [line]
  (let [{rule :rule pw :pw} (line->rule+password line)
        {mn :min mx :max chr :char} (parse-rule->map rule)
        cc (password->char-count pw chr)]
     (<= mn cc mx)))


(defn valid-password-part-two? [line]
  (let [{rule :rule pw :pw} (line->rule+password line)
        {mn :min mx :max chr :char} (parse-rule->map rule)
        char-pos (password->char-positions pw chr)
        fp (char-pos mn)
        sp (char-pos mx)]
    (and (or fp sp) (or (not fp) (not sp)))))
