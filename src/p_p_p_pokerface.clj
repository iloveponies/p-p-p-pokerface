(ns p-p-p-pokerface)

(defn rank [card]
  "card is a string with values for rank and suit."
  (let [[rank-char _] card
        rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (get rank-map rank-char))))

(defn suit [card]
  "card is a string with values for rank and suit."
  (let [[_ suit-char] card]
    (str suit-char)))

(defn rank-freq-map [hand]
  (frequencies (map rank hand)))

(defn suit-freq-map [hand]
  (frequencies (map suit hand)))

(defn pair? [hand]
  (let [rank-counts (frequencies (map rank hand))]
    (if (> (apply max (vals rank-counts)) 1)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [rank-counts (frequencies (map rank hand))]
    (if (= (apply max (vals rank-counts)) 3)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [rank-counts (rank-freq-map hand)]
    (if (= (apply max (vals rank-counts)) 4)
      true
      false)))

(defn flush? [hand]
  (let [hand-suits (suit-freq-map hand)]
    (if (= (apply max (vals hand-suits)) 5)
      true
      false)))

(defn full-house? [hand]
  (if (three-of-a-kind? hand)
    ; check to see if the hand has an additional pair
    (let [rank-freqs-map (frequencies (map rank hand))
          freq-set (set (vals rank-freqs-map))]
      (contains? freq-set 2))
    false))

(defn two-pairs? [hand]
  (if (four-of-a-kind? hand)
    true
    (let [curr-pairs (filter
                      (fn [f-entry] (= (val f-entry) 2))
                      (rank-freq-map hand))]
      (= (count curr-pairs) 2))))

(defn straight? [hand]
  (let [sorted-hand-ranks (sort (map rank hand))
        low-card (first sorted-hand-ranks)]
    (if (= sorted-hand-ranks (range low-card (+ low-card 5)))
      true
      ; Check for an ace!
      (let [first-four (take 4 sorted-hand-ranks)]
        (if (contains? (set sorted-hand-ranks) 14)
        ; put a low ace on front and check again!
        (if (= (cons 1 first-four) (range 1 (+ 1 5)))
          true
          false
          )
        false)))))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))

(defn high-card? [hand]
  true)

(defn value [hand]
  ; checkers is a set of functions and values
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]
                   }
        ; Create a function that applies a checker to a hand.
        hand-value (fn [checker]
                     (let [[matcher value] checker]
                       (if (matcher hand)
                         value
                         ; otherwise, set it to a negative value!
                         -1)))]
    ; Map this function accross all checkers and grabe the max value.
    (apply max (map hand-value checkers))
    ))

; TESTING DEFS!
;(def high-seven ["2H" "3S" "4C" "5C" "7D"])
;(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
;(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
;(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
;(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
;(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
;(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
;(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
;(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
;(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
;(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
;(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
;(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])
