(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        vals {\T 10,\J 11,\Q 12,\K 13,\A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get vals fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        most (apply max (vals (frequencies ranks)))]
    (= most 2)))

(defn three-of-a-kind? [hand]
    (let [ranks (map rank hand)
        most (apply max (vals (frequencies ranks)))]
    (= most 3)))

(defn four-of-a-kind? [hand]
    (let [ranks (map rank hand)
        most (apply max (vals (frequencies ranks)))]
    (= most 4)))

(defn flush? [hand]
    (let [suits (map suit hand)
        most (apply max (vals (frequencies suits)))]
    (= most 5)))

(defn full-house? [hand]
    (let [ranks (map rank hand)
          rank-list (vals (frequencies ranks))
          most (apply max rank-list)
          few (apply min rank-list)]
    (= most 2)))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
