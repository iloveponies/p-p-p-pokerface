(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\A 14,
                     \K 13,
                     \Q 12,
                     \J 11
                     \T 10})
  (let [[a b] card]
  (cond
    (Character/isDigit a)(Integer/valueOf (str a))
    :else  (replacements a))))

(defn suit [card]
  (let [[a b] card]
  (str b)))

(defn pair? [hand]
  (< 1
     (apply max
            (vals
              (frequencies
                (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2
     (apply max
            (vals
              (frequencies
                (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3
     (apply max
            (vals
              (frequencies
                (map rank hand))))))

(defn flush? [hand]
  (== 1
      (count
        (vals
          (frequencies
            (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort
             (vals
               (frequencies
                 (map rank hand))))))

(defn two-pairs? [hand]
  (let [list (filter (fn [x] (== 0 (mod x 2)))
                      (vals (frequencies (map rank hand))))]
    (cond
      (= 2 (count list)) true
      (= 4 (first list)) true
      :else false)))

(defn straight? [hand]
  (let [handRanks (sort (map rank hand))]
    (cond
      (= (range (apply min handRanks)
                (+ 1 (apply max handRanks)))
        handRanks) true
      (= [2 3 4 5 14] handRanks) true
      :else false)))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

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
