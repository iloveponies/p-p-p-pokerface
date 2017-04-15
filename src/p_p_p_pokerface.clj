(ns p-p-p-pokerface)

(def char-to-card {\T 10, \J 11, \Q 12, \K 13 \A 14})

(defn rank [card]
  (let [[rank _] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (char-to-card rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (not (empty?
         (filter (fn [x] (= 2 x)) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (empty?
         (filter (fn [x] (= 3 x)) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (not (empty?
         (filter (fn [x] (= 4 x)) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= 2 (count (filter (fn [x] (= 2 x)) (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [converted_hand   (replace {14 1} (map rank hand)) ; we don't need no stinkin' suits
        sorted_orig      (sort (map rank hand))
        sorted_converted (sort converted_hand)
        first_orig       (first sorted_orig)
        first_converted  (first sorted_converted)]
    (or (= sorted_orig (take 5 (iterate inc first_orig))) (= sorted_converted (take 5 (iterate inc first_converted))) ) ))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush?  hand) 8
    (four-of-a-kind?  hand) 7
    (full-house?      hand) 6
    (flush?           hand) 5
    (straight?        hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs?       hand) 2
    (pair?            hand) 1
    :else                   0))
