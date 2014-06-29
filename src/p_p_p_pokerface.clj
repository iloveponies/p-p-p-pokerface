(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r s] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (cond
       (= r \T) 10
       (= r \J) 11
       (= r \Q) 12
       (= r \K) 13
       (= r \A) 14
       true nil))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (let [[count] (vals (frequencies (map suit hand)))]
    (== 5 count)))
    

(defn full-house? [hand]
  (let [[a b] (vals (frequencies (map rank hand)))]
    (or (and (== a 3) (== b 2))
        (and (== a 2) (== b 3)))))

(defn two-pairs? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (or
     (and (== 3 (count freqs))
          (== 2 (apply max freqs)))
     (== 4 (apply max freqs)))))

(defn straight? [hand]
  (let [freq (frequencies (map rank hand))
        low-freq (frequencies (replace {14 1} (map rank hand)))
        test-straight (fn [freq] 
                        (and
                         (== (count freq) 5)
                         (== 4 (- (apply max (keys freq))
                                  (apply min (keys freq))))))]
    (or
     (test-straight freq)
     (test-straight low-freq))))


(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
   true 0))
