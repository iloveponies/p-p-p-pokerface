(ns p-p-p-pokerface)

(defn rank [card]
  (let [face (first card)]
    (cond (Character/isDigit face) (Integer/valueOf (str face))
          (= \T face) 10
          (= \J face) 11
          (= \Q face) 12
          (= \K face) 13
          (= \A face) 14)))

(defn suit [card]
  (str (first (rest card))))

(defn of-a-kind [f hand]
  "Returns the number of occurences of a rank/suit in a hand"
  (apply max
         (vals
           (frequencies
             (map #(f %) hand)))))

(defn pair? [hand]
  (if (>= (of-a-kind rank hand) 2)
    true
    false))

(defn three-of-a-kind? [hand]
  (if (>= (of-a-kind rank hand) 3)
    true
    false))

(defn four-of-a-kind? [hand]
  (if (>= (of-a-kind rank hand) 4)
    true
    false))

(defn flush? [hand]
  (if (>= (of-a-kind suit hand) 4)
    true
    false))

(defn full-house? [hand]
  (= #{2 3}
     (set (vals
            (frequencies
              (map #(rank %) hand))))))

(defn two-pairs? [hand]
  (if (= 2 ((frequencies (vals
                           (frequencies
                             (map #(rank %) hand)))) 2))
    true
    false))

(defn straight? [hand]
  (let [hand-set (set (map #(rank %) hand))
        ace-low-hand-set (set (replace {14 1} hand-set))]
    (cond (< (count hand-set) 5) false
          :else
          (or
            (= hand-set (set (range
                               (apply min hand-set)
                               (inc (apply max hand-set)))))
            (= ace-low-hand-set (set (range
                                       (apply min ace-low-hand-set)
                                       (inc (apply max ace-low-hand-set)))))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
