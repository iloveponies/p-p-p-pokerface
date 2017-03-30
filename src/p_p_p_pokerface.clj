(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
    replacements {\A 14, \K 13, \Q 12, \J 11, \T 10}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacements r))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn pair? [hand]
  (let [same-ranks (apply max (vals (frequencies (map rank hand))))]
    (if (> same-ranks 1)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [same-ranks (apply max (vals (frequencies (map rank hand))))]
    (if (> same-ranks 2)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [same-ranks (apply max (vals (frequencies (map rank hand))))]
    (if (> same-ranks 3)
      true
      false)))

(defn flush? [hand]
  (let [same-suit (apply max (vals (frequencies (map suit hand))))]
    (if (= same-suit 5)
      true
      false)))

(defn full-house? [hand]
  (let [ranks (sort (vals (frequencies (map rank hand))))]
    (if (= ranks (seq [2 3]))
      true
      false)))

(defn two-pairs? [hand]
  (let [ranks (sort (vals (frequencies (map rank hand))))]
    (if (or (= ranks (seq [1 2 2])) (= ranks (seq [1 4])))
      true
      false)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks-min (apply min ranks)
        ranks-max (apply max ranks)
        ranks-small-ace (sort (replace {14 1} ranks))
        ranks-small-ace-min (apply min ranks-small-ace)
        ranks-small-ace-max (apply max ranks-small-ace)]
    (if (or (= ranks (range ranks-min (+ ranks-max 1)))
            (= ranks-small-ace (range ranks-small-ace-min (+ ranks-small-ace-max 1))))
      true
      false)))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
  (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
