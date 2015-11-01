(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn suit [card]
  "Exercise 1: 
   Write the function (suit card) which takes a single card 
   and returns the suit of the card as a one character string."
  (let [[_ snd] card]
    (str snd)))

(defn rank [card]
  "Exercise 2: 
   Write the function (rank card) which takes a single card 
   and returns the rank as a number between 2 and 14."
  (let [[fst _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn ranks-frequencies [hand]
  "Helper Function."
  (let [ranks (map (fn [card] (rank card)) hand)]
    (vals (frequencies ranks))))

(defn pair? [hand]
  "Exercise 3:
   Write the function (pair? hand) that returns true if there
   is a pair in hand and false if there is no pair in hand."
  (= (apply max (ranks-frequencies hand)) 
      2))

(defn three-of-a-kind? [hand]
  "Exercise 4:
   Write the function (three-of-a-kind? hand) that returns true if 
   the hand contains a three of a kind."
  (= (apply max (ranks-frequencies hand))
      3))

(defn four-of-a-kind? [hand]
  "Exercise 5:
   Write the function (four-of-a-kind? hand) that returns true if
   the hand contains a four of a kind."
  (= (apply max (ranks-frequencies hand))
      4))

(defn flush? [hand]
  "Exercise 6:
   Write the function (flush? hand) that returns true if the hand is a flush."
  (= (count (set (map (fn [card] (suit card)) hand)))
      1))

(defn full-house? [hand]
  "Exercise 7:
   Write the function (full-house? hand) that returns true if hand is a full house,
   and otherwise false"
  (let [category (into [] (sort (ranks-frequencies hand)))]
    (= category
       [2 3])))

(defn two-pairs? [hand]
  "Exercise 8:
   Write the function (two-pairs? hand) that return true if hand has two pairs,
   and otherwise false."
   (let [category (into [] (sort (ranks-frequencies hand)))] 
     (or (= category [1 2 2])
         (= category [1 4]))))

(defn straight? [hand]
  "Exercise 9:
   Write the function (straight? hand) that returns true if hand is a straight,
   and otherwise false."
  (let [old_ranks (into [] (map (fn [card] (rank card)) hand))
        new_ranks (replace {14 1} old_ranks)
        cmp_ranks (fn [ranks] (range (apply min ranks) (+ (apply max ranks) 1)))]
    (or  (= (cmp_ranks old_ranks) (sort old_ranks))
         (= (cmp_ranks new_ranks) (sort new_ranks)))))

(defn straight-flush? [hand]
  "Exercise 10:
   Write the function (straight-flush? hand) which returns true if the hand
   is a straight flush, that is both a straight and a flush, and otherwise false."
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  "All hands have a high card"
  true)

(defn value [hand]
  "Exercise 11:
   Write the function (value, hand), which returns the value of a hand according to 
   the table above."
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map (fn [pair] (second pair)) (filter (fn [pair] (first pair)) (map (fn [checker] (vector (apply (first checker)[hand]) (second checker))) checkers))))))
