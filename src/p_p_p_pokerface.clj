(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst snd] card
        non-number {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst) (Integer/valueOf (str fst)) (get non-number fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn freq? [hand f]
  (vals (frequencies (map f hand))))

(defn same-ranks? [hand n]
 (if (>= (apply max (freq? hand rank)) n) true false))

(defn pair? [hand]
  (same-ranks? hand 2))


(defn three-of-a-kind? [hand]
  (same-ranks? hand 3))

(defn four-of-a-kind? [hand]
  (same-ranks? hand 4))

(defn same-suits? [hand n]
  (if (>= (apply max (freq? hand suit)) n) true false))

(defn flush? [hand]
  (same-suits? hand 5))

(defn full-house? [hand]
  (let [[fst snd] (freq? hand rank)]
    (if (and
         (== (+ fst snd) 5)
         (not= fst 1)
         (not= snd 1)) true false)))

(defn two-pairs? [hand]
  (let [[fst snd] (freq? hand rank)]
    (if (and
         (== (+ fst snd) 4)
         (not= fst 1)
         (not= snd 1)) true false)))


(defn same-range-as-hand? [r1 hand]
  (= (range r1 (+ r1 5)) hand))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        sorted2 (sort (replace {14 1} sorted))]
    (or
     (same-range-as-hand? (first sorted) sorted)
     (same-range-as-hand? (first sorted2) sorted2))))

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
