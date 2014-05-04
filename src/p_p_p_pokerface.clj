(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-of-a-kind? [hand n]
  (if (= n (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [ranks (map suit hand)]
    (if (= 5 (nth (vals (frequencies ranks)) 0))
      true
      false)))

(defn full-house? [hand]
  (if (and (n-of-a-kind? hand 3) (apply (= 2) (vals (frequencies (map rank hand)))))
    true
    false))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        lowest (nth sorted-hand 0)
        highest (nth sorted-hand 4)]
    (if (= highest (+ lowest 4))
      true
      false)))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
