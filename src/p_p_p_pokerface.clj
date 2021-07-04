(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst] card
        ranks {\T 10
               \J 11
               \Q 12
               \K 13
               \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (ranks fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn pair? [hand]
  (let [ranks (map rank hand)
        frequency (frequencies ranks)
        values (vals frequency)
        checker (fn [x] (== x 2))]
        ;max-val (apply max values)]
    ;(if (== max-val 2) true false)))
    (not (empty? (filter checker values)))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        frequency (frequencies ranks)
        values (vals frequency)
        checker (fn [x] (== x 3))]
    (not (empty? (filter checker values)))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        frequency (frequencies ranks)
        values (vals frequency)
        checker (fn [x] (== x 4))]
    (not (empty? (filter checker values)))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        frequency (frequencies ranks)
        values (vals frequency)
        checker (fn [x] (== x 2))]
    (or (== (count (filter checker values)) 2) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        rankslow (sort (replace {14 1} ranks))
        low (apply min ranks)
        high (apply max ranks)
        str1 (range low (+ low 5))
        str2 (range 1 6)]
    (or (= ranks str1) (= rankslow str2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        helper (fn [x] ((first x) hand))
        all-values (map second (filter helper checkers))]
    (apply max all-values)))

; *___*
