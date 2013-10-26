(ns p-p-p-pokerface)

(defn rank [card]
  (let [[x _] card]
  (if (Character/isDigit x)
    (Integer/valueOf (str x))
    (get {\T 10, \J 11, \Q 12, \K 13, \A 14} x))))

(defn suit [card]
  (let [[_ st] card]
  (str st)))

(defn pair? [hand]
  (let [hand (map rank hand)
        freq (vals (frequencies hand))]
  (contains? (set freq) 2)))

(defn three-of-a-kind? [hand]
    (let [hand (map rank hand)
        freq (vals (frequencies hand))]
  (contains? (set freq) 3)))

(defn four-of-a-kind? [hand]
    (let [hand (map rank hand)
        freq (vals (frequencies hand))]
  (contains? (set freq) 4)))

(defn flush? [hand]
    (let [hand (map suit hand)
        freq (vals (frequencies hand))]
  (contains? (set freq) 5)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [hand (map rank hand)
        freq (vals (frequencies hand))]
    (== (count (filter (fn [x] (== 2 x)) freq)) 2)))

(defn straight [first]
  (range first (+ first 5)))

(defn straight? [hand]
  (let [sortedhand (sort (map rank hand))
        firstinhand (first sortedhand)
        lastinhand (last sortedhand)
        straighthand (straight firstinhand)]

    (or (= sortedhand straighthand)
        (= (straight 1) (sort (replace {14 1} sortedhand))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

;(defn high-card? [hand]
;  true)

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

