(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [card]
  (let [[fst _] card]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (get {\A 14, \K 13, \T 10
          \Q 12, \J 11} fst))))

(defn suit [card]
 (let [[_ snd] card]
   (str snd)))

(defn count-card-frequencies [hand]
 (vals (frequencies (map #(rank %) hand))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
 (if (empty?
       (filter #(= 2 %)
       (count-card-frequencies hand)))
    false true))

(defn three-of-a-kind? [hand]
 (if (empty?
       (filter #(= 3 %)
       (count-card-frequencies hand)))
    false true))

(defn four-of-a-kind? [hand]
 (if (empty?
       (filter #(= 4 %)
       (count-card-frequencies hand)))
    false true))

(defn flush? [hand]
 (if (empty?
       (filter #(= 5 %)
       (vals (frequencies (map #(suit %) hand)))))
    false true))

(defn full-house? [hand]
 (and (pair? hand)
      (three-of-a-kind? hand)))

(defn two-pairs? [hand]
 (= [1 2 2] (sort (count-card-frequencies hand))))


(defn straight? [hand]
  (let [ranks (sort (map #(rank %) hand))
        fixed (if (= [2 3 4 5 14] ranks)
                     [1 2 3 4 5] ranks)]
    (and (= 4 (- (last fixed) (first fixed)))
         (= 5 (count (distinct fixed))))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn hand-has-value? [hand checker-value]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
   (not (empty?(filter (fn [x] (and ((first x) hand)
                       (= checker-value (second x)))) checkers)))))

(defn value [hand]
  (let [v (range 9)]
    (apply max (filter (fn [x] (hand-has-value? hand x)) v))))

