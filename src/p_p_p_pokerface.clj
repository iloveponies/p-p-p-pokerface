(ns p-p-p-pokerface)


(def rank-map {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[first _] card]
  (if (Character/isDigit first)
    (Integer/valueOf (str first))
    (rank-map first)
  )))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))


(defn max-of-a-rank-in-hand [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (let [value (max-of-a-rank-in-hand hand)]
    (> value 1)))

(defn three-of-a-kind? [hand]
  (let [value (max-of-a-rank-in-hand hand)]
    (> value 2)))

(defn four-of-a-kind? [hand]
 (let [value (max-of-a-rank-in-hand hand)]
    (> value 3)))

(defn flush? [hand]
    (apply = (map suit hand)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= [2 3] (sort freqs))))


(defn num-pairs? [hand]
  (let [ ranks (map rank hand)
         freqs (vals (frequencies ranks))]
    (count (filter #{2} freqs))))

(defn two-pairs? [hand]
  (>= (num-pairs? hand) 2))


(defn straight? [hand]
  (let [ace-as-high (sort (map rank hand))
        ace-as-low (sort (replace {14 1} ace-as-high))]
    (or (= ace-as-high (range (apply min ace-as-high) (+ 1 (apply max ace-as-high))))
        (= ace-as-low (range (apply min ace-as-low) (+ 1 (apply max ace-as-low)))))))


;(straight? two-pairs-hand)


(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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

