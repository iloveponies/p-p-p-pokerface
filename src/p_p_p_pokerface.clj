(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank (let [[fst _] card] fst)]
  (cond (Character/isDigit rank)(Integer/valueOf (str rank))
        :else (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (str (let [[_ snd] card] snd)))

(defn card-rank [x]
  (let [[fst _] x] fst))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals(frequencies (map suit hand))))))

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
