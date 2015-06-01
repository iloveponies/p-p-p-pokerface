(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r] card
        faces {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get faces r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (not (empty? (filter #(= % 2) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (empty? (filter #(= % 3) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (not (empty? (filter #(= % 4) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (not (empty? (filter #(= % 5) (vals (frequencies (map suit hand)))))))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand))))
     '(2 3)))

(defn two-pairs? [hand]
  (or (= (filter #(= % 2) (vals (frequencies (map rank hand))))
         '(2 2))
      (= (filter #(= % 4) (vals (frequencies (map rank hand))))
         '(4))))

(defn straight? [hand]
  (let [h (sort (keys (frequencies (map rank hand))))]
    (cond (not (= (count h) 5)) false
          (= (- (last h) (first h)) 4) true
          true (and (= (first h) 2) (= (last h) 14)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        true 0))
