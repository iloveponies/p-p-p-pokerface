(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank 
  "Returns the rank of a card." 
  [card]
  (let [[r] card]
    (cond
      (Character/isDigit r) (Integer/valueOf (str r))
      :else (replacements r))))

(defn suit 
  "Returns the suit of a card." 
  [card]
  (let [[_ s] card]
  (str s)))

(defn pair? 
  "Returns true if there is a pair in the hand." 
  [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? 
  "Returns true if there is three of a kind in the hand." 
  [hand]
  (== 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? 
  "Returns true if there is four of a kind in the hand." 
  [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? 
  "Returns true if there is a flush in the hand."
  [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? 
  "Returns true if there is a full house in the hand." 
  [hand]
  (= (sort (vals (frequencies (map rank hand)))) (range 2 4)))

(defn two-pairs? 
  "Returns true if there are two pairs in the hand." 
  [hand]
  (cond
    (= (sort (vals (frequencies (map rank hand)))) (seq [1 4])) true
    (= (sort (vals (frequencies (map rank hand)))) (seq [ 1 2 2])) true
    :else false))

(defn straight? 
  "Returns true if the hand is a straight." 
  [hand]
  (let [sorted_hand (sort (map rank hand))]
    (if (== 14 (apply max sorted_hand)) 
      (if (= (sort (replace {14 1} (map rank hand))) (range 1 6))
        true;low ace straight
        (if (= (sort (map rank hand)) (range 10 15))
          true;high ace straight
          false));no straight
      (= sorted_hand (range (apply min sorted_hand) (+ (apply min sorted_hand) 5)))))) ;no ace

(defn straight-flush? 
  "Returns true if the hand is a straight flush." [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? "Returns true if the hand has a high card (always true)." [hand]
  true)

(defn value 
  "Returns the value of a hand."
  [hand]
  (let [checkers #{[high-card? 0]       [pair? 1]
                   [two-pairs? 2]       [three-of-a-kind? 3]
                   [straight? 4]        [flush? 5]
                   [full-house? 6]      [four-of-a-kind? 7]
                   [straight-flush? 8]}]
  (apply max (map second (filter (fn [checker] ((first checker) hand)) checkers)))))
