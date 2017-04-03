(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        letter-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (letter-ranks rank))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (not (not-any? #(= 2 %) (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (not (not-any? #(= 3 %) (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (not (not-any? #(= 4 %) (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (not (not-any? #(= 5 %) (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (vec (sort (vec (vals (frequencies (map rank hand))))))))

(defn two-pairs? [hand]
  (if (four-of-a-kind? hand)
    true
    (= [2 2] (vec (filter #(= 2 %) (vals (frequencies (map rank hand))))))))

(defn straight? [hand]
  (let [rank-hand (vec (sort (map rank hand)))
        rank-hand-low-ace (vec (sort (replace {14 1} rank-hand)))]
    (if (= (vec (range (first rank-hand) (+ (last rank-hand) 1))) rank-hand)
      true
      (= (vec (range (first rank-hand-low-ace) (+ (last rank-hand-low-ace) 1)))
         rank-hand-low-ace))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let  [checkers #{[high-card? 0]  [pair? 1]
                    [two-pairs? 2]  [three-of-a-kind? 3]
                    [straight? 4]   [flush? 5]
                    [full-house? 6]  [four-of-a-kind? 7]
                    [straight-flush? 8]}]
    
    (apply max (filter #(not (nil? %)) (map #(if ((first %) hand) (second %)) checkers)))))

