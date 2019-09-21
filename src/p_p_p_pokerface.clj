(ns p-p-p-pokerface)

(def facecard {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (facecard r))))

(defn suit [card]
 (str (get card 1)))

(defn pair? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (== (count (vals (frequencies (map suit hand)))) 1))

(defn full-house? [hand]
  (let [rankvals (vals (frequencies (map rank hand)))]
    (and (= (apply max rankvals) 3) (= (apply min rankvals) 2))))

(defn two-pairs? [hand]
  (if (four-of-a-kind? hand) true
    (and (pair? hand) 
         (== (count (vals (frequencies (map rank hand)))) 3))))

(defn straight? [hand]
  (let [high-ace-hand? (fn [x] (> (apply min x) 9))
        low-ace (fn [x] (sort (replace {14 1} x)))
        ranks (fn [x] (sort (map rank x)))]
    (if (== (count (set (ranks hand ))) 5)
      (if (high-ace-hand? (ranks hand))
        (== (- (apply max (ranks hand)) (apply min (ranks hand))) 4)
        (== (- (apply max (low-ace (ranks hand))) 
               (apply min (low-ace (ranks hand)))) 4))
      false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  ; A naive but wokring implementation
    (if (straight-flush? hand) 8
      (if (four-of-a-kind? hand) 7
        (if (full-house? hand) 6
          (if (flush? hand) 5
            (if (straight? hand) 4
              (if (three-of-a-kind? hand) 3
                (if (two-pairs? hand) 2
                  (if (pair? hand) 1
                    (if (high-card? hand) 0))))))))))
