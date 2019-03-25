(ns p-p-p-pokerface)

(def face-card-values {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank-char _] card]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (face-card-values rank-char))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn max-with-same-rank [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (= 2 (max-with-same-rank hand)))

(defn three-of-a-kind? [hand]
  (= 3 (max-with-same-rank hand)))

(defn four-of-a-kind? [hand]
  (= 4 (max-with-same-rank hand)))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn min-rank [hand]
  (apply min (map rank hand)))


(defn max-rank [hand]
  (apply max (map rank hand)))

(defn straight? [hand]
  (let [test-straight
    (fn [ranks] (= (sort ranks)
      (range (apply min ranks) (+ 1 (apply max ranks)))))]
  (or
    (test-straight (map rank hand))
    (test-straight (replace {14 1} (map rank hand))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers  #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3]
                    [straight? 4] [flush? 5] [full-house? 6] [four-of-a-kind? 7]
                    [straight-flush? 8]}
        test-hand   (fn [checker] ((first checker) hand))]
  (apply max (map second (filter test-hand checkers)))))
