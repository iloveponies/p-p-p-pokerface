(ns p-p-p-pokerface)

(def ranks {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get ranks r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (not (empty? (filter (fn [val] (>= val 2)) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (empty? (filter (fn [val] (>= val 3)) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
 (not (empty? (filter (fn [val] (>= val 4)) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
  (and (= (count freqs) 2)
       (= (apply max freqs) 3))))

(defn two-pairs? [hand]
  (if (four-of-a-kind? hand) true
    (let [freqs (vals (frequencies (map rank hand)))]
      (and (= (count freqs) 3)
         (= (apply max freqs) 2)))))

(defn straight? [hand]
  (let [low (sort (replace {14 1} (map rank hand)))
        high (sort (map rank hand))]
    (cond
     (= low (range (first low) (+ (first low) 5))) true
     (= high (range (first high) (+ (first high) 5))) true
     :else false)))

(defn straight-flush? [hand]
  (and
   (flush? hand)
   (straight? hand)))

(comment defn value [hand]
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

(defn high-card? [hand]
  true)
   
(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3] [straight? 4]
                   [flush? 5] [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}
        scores (map second (filter (fn [checker] ((first checker) hand)) checkers))]
    (apply max scores)))
    
   
