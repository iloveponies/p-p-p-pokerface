(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        r {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (contains? r fst)
      (get r fst)
      (Integer/valueOf (str fst)))))

(defn suit [card]
  (let [[fst snd] card]
  (str snd)))

(defn pair? [hand]
   (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
 (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (let [s (vals (frequencies (map suit hand)))]
  (= (str "(5)") (str s))))

(defn full-house? [hand]
  (let [[x y] (sort (vals (frequencies (map rank hand))))]
   (and (== 2 x) (== 3 y))))

(defn two-pairs? [hand]
  (let [[x y] (sort (vals (frequencies (map rank hand))))]
   (< 1 y)))

(defn straight? [hand]
  (if (pair? hand)
    false
    (let [[x y z t v] (sort (map rank hand))]
      (if (= 4 (+ (- v t) (- t z) (- z y) (- y x)))
        true
        (if (= 4 (+ (- t z) (- z y) (- y x) (- x (- v  13))))
          true
          false)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (if (straight-flush? hand)
    8
    (if (four-of-a-kind? hand)
      7
      (if (full-house? hand)
        6
        (if (flush? hand)
          5
          (if (straight? hand)
            4
            (if (three-of-a-kind? hand)
              3
              (if (two-pairs? hand)
                2
                (if (pair? hand)
                  1
                  0)))))))))