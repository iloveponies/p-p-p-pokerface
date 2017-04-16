(ns p-p-p-pokerface)

; Numeric value of the card
(defn rank [card]
  (let [[value _] card
        symbols {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (cond
      (and (Character/isDigit value) (< 1 (Integer/valueOf (str value)) 10)) (Integer/valueOf (str value))
      (and (not (Character/isDigit value)) (contains? symbols value)) (get symbols value)
      :else nil
      )
    )
  )

; Suit of the card as a String
(defn suit [card]
  (let [[_ sui] card]
    (str sui)
    )
  )

; Helper method to get number of same values in hand.
(defn number-of-same-values-in-hand [hand]
  (apply max (vals (frequencies (map rank hand))))
  )

; Helper method to check if element belongs to collection
(defn in? [coll elm]
  (some #(= elm %) coll)
  )

; There's pair in the hand (max)
(defn pair? [hand]
  (< 1 (number-of-same-values-in-hand hand))
  )

; There's three in the hand (max)
(defn three-of-a-kind? [hand]
  (< 2 (number-of-same-values-in-hand hand)))

; There's four in the hand (max)
(defn four-of-a-kind? [hand]
  (< 3 (number-of-same-values-in-hand hand)))

; All suits are the same
(defn flush? [hand]
  (let [[card1 _ _ _ _] hand
        tezt (fn [card] (= (suit card) (suit card1)))]
    (not (in? (map tezt hand) false))
    )
  )

; Hand includes a full house
(defn full-house? [hand]
  (let [x (vals (frequencies (map rank hand)))]
    (true? (and (in? x 2) (in? x 3)))
  ))

; Two pairs - note that four of a kind is considered to be two pairs also...
; according to rules of the exercise.
(defn two-pairs? [hand]
  (let [x (vals (frequencies (map rank hand)))]
    (or (four-of-a-kind? hand) (== 2 (get (frequencies x) 2 0)))
  ))

; Check if hand is straght
(defn straight? [hand]
  (let [cards (sort (map rank hand))
        ; edit cards - if there's ace and 2, convert ace to 1.
        cards_edited (if (and (in? cards 14) (in? cards 2))
                       (filter (fn [x] (not(== x 14))) (conj cards 1))
                       cards)
        [val1 val2 val3 val4 val5] cards_edited]
    (< val1 val2 val3 val4 val5)
    )
  )

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
