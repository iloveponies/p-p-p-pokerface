(ns p-p-p-pokerface)

(defn rank [card]
  (let [[val _] card face {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit val)
      (Integer/valueOf (str val))
      (face val))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [values (map rank hand)
        card-combinations (frequencies values)
        card-matches (vals card-combinations)
        max-matches (apply max card-matches)]
    (if (and (== max-matches 2) (== (count card-matches) 4))
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [values (map rank hand)
        card-combinations (frequencies values)
        card-matches (vals card-combinations)
        max-matches (apply max card-matches)]
    (if (and (== max-matches 3) (== (count card-matches) 3))
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [values (map rank hand)
        card-combinations (frequencies values)
        card-matches (vals card-combinations)
        max-matches (apply max card-matches)]
    (if (== max-matches 4)
      true
      false)))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-combinations (frequencies suits)]
    (if (== (count (keys suit-combinations)) 1)
      true
      false)))

(defn full-house? [hand]
  (let [values (map rank hand)
        card-combinations (frequencies values)
        card-matches (vals card-combinations)
        max-matches (apply max card-matches)]
    (if (and (== max-matches 3) (== (count card-matches) 2))
      true
      false)))

(defn two-pairs? [hand]
  (let [values (map rank hand)
        card-combinations (frequencies values)
        card-matches (vals card-combinations)
        max-matches (apply max card-matches)]
    (if (and (== max-matches 2) (== (count card-matches) 3))
      true
      false)))

(defn straight? [hand]
  (let [high-values (sort (map rank hand))
        low-values (sort (replace {14 1} high-values))
        card-combinations (frequencies high-values)
        high-start (first high-values)
        high-straight (range high-start (+ high-start 5))
        low-start (first low-values)
        low-straight (range low-start (+ low-start 5))]
    (if (or (= high-values high-straight)
            (= low-values low-straight))
      true
      false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
