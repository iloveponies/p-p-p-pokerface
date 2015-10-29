(ns p-p-p-pokerface)

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

; low-ace-straight-hand [2H 3S 4C 5D AD]
; straight-hand [2H 3S 6C 5D 4D]
; high-ace-straight-hand [TH AS QC KD JD]
(defn straight? [hand]
  "Exercise 9:
   Write the function (straight? hand) that returns true if hand is a straight,
   and otherwise false.")

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
