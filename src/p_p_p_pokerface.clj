(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        ch (first card)]
    (if (Character/isDigit ch)
      (Integer/valueOf (str ch))
      (replacements ch))))

(defn suit [card]
  (str (second card)))

(defn n-of-a-kind? [n hand]
  (let [ranks (map rank hand)]
  (>= (apply max (vals (frequencies ranks))) n)))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== 1 (count (set suits)))))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (and (== 2 (apply min (vals (frequencies ranks))))
         (== 3 (apply max (vals (frequencies ranks)))))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (cond
     (four-of-a-kind? hand) true
     (full-house? hand) true
     (and (not (three-of-a-kind? hand)) (== 3 (count (frequencies ranks)))) true
     :else false)))
 
(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
