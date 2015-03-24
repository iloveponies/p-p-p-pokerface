(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
    (let [[first _] card]
      (if (Character/isDigit first)
        (Integer/valueOf (str first))
        (replacements first))))

(defn suit [card]
  (let [[_ second] card]
    (str second)))

(defn pair? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (== (apply min (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (not (four-of-a-kind? hand))
       (== (count (vals (frequencies (map rank hand)))) 2)))

(defn two-pairs? [hand]
  (or (and (not (three-of-a-kind? hand))
           (== (count (vals (frequencies (map rank hand)))) 3))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [handvals (map rank hand)
        handvalsb (replace {14 1} handvals)]
    (or (= (sort handvals)
           (range (apply min handvals)
                  (+ (apply min handvals) 5)))
        (= (sort handvalsb)
           (range (apply min handvalsb)
                  (+ (apply min handvalsb) 5))))))

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
