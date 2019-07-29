(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn freq [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (>= (freq hand) 2))

(defn three-of-a-kind? [hand]
  (>= (freq hand) 3))

(defn four-of-a-kind? [hand]
  (>= (freq hand) 4))

(defn flush? [hand]
  (>= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (or (= (sort (vals (frequencies (map rank hand)))) [1 2 2]) (four-of-a-kind? hand)))

(defn xer [hand]
  (sort (map rank hand)))

(defn min-seq [hand]
  (map (fn [x] (- x (apply min (xer hand)))) (xer hand)))

(defn straight? [hand]
 (or (= (min-seq hand) [0 1 2 3 4]) (= (min-seq hand) [0 1 2 3 12])))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [hand-vals #{[high-card? 0] [pair? 1]
                    [two-pairs? 2] [three-of-a-kind? 3]
                    [straight? 4] [flush? 5]
                    [full-house? 6] [four-of-a-kind? 7]
                    [straight-flush? 8]}
        checker #(if ((first %) hand) (second %) 0)]
    (apply max (map checker hand-vals))))
