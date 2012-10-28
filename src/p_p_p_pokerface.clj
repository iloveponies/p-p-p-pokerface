(ns p-p-p-pokerface)

(def r-conv {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [[r _]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (r-conv r)))

(defn suit [[_ s]]
  (str s))

(defn freq-hand [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (if (= (count (filter #(= 2 %) (freq-hand hand))) 1)
    true
    false))

(defn three-of-a-kind? [hand]
  (if (= (count (filter #(= 3 %) (freq-hand hand))) 1)
    true
    false))

(defn four-of-a-kind? [hand]
  (if (= (count (filter #(= 4 %) (freq-hand hand))) 1)
    true
    false))

(defn flush? [hand]
  (if (apply = (map suit hand))
    true
    false))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (if (or (= (count (filter #(= 2 %) (freq-hand hand))) 2)
          (four-of-a-kind? hand))
    true
    false))

(defn straight? [hand]
  (let [ranks (vec (map rank hand))]
  (if (and (= (count ranks) (count (distinct ranks)))
           (or (= (- (apply max ranks) (apply min ranks)) 4)
               (= (sort ranks) [2, 3, 4, 5, 14]))) ; low ace case
    true
    false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))

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
