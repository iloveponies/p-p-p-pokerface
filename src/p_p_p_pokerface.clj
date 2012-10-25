(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        high-cards {\A 14, \K 13, \Q 12, \J 11 \T 10}]
    (if (Character/isDigit r) (Integer/valueOf (str r)) (get high-cards r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [frqs (vals (frequencies (map rank hand)))
        twos (filter (fn [x] (== x 2)) frqs)]
    (not (empty? twos))))

(defn three-of-a-kind? [hand]
  (let [frqs (vals (frequencies (map rank hand)))
        trios (filter (fn [x] (== x 3)) frqs)]
    (not (empty? trios))))

(defn four-of-a-kind? [hand]
  (let [frqs (vals (frequencies (map rank hand)))
        fours (filter (fn [x] (== x 4)) frqs)]
    (not (empty? fours))))

(defn flush? [hand]
  (let [frqs (frequencies (map suit hand))]
    (== (count frqs) 1)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [frqs (vals (frequencies (map rank hand)))
        twos (filter (fn [x] (== x 2)) frqs)]
    (or (== (count twos) 2) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ace-high (sort (map rank hand))
        [a b c d e] ace-high
        ace-low (sort (replace {14 1} ace-high))
        [v w x y z] ace-low 
        plus-one (fn [g h] (= h (+ g 1)))]
    (or
     (and (plus-one a b) (plus-one b c) (plus-one c d) (plus-one d e))
     (and (plus-one v w) (plus-one w x) (plus-one x y) (plus-one y z)))))

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