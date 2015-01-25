(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (cond (Character/isDigit fst)
      (Integer/valueOf (str fst))
      :else ({\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
