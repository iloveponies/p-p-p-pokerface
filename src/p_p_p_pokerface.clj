(ns p-p-p-pokerface)


;Functions dealing with hands and cards:
(defn rank [card]
  "Helper function that returns the rank of a single card"
  (let [[char] card
        values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit char)
      (Integer/valueOf (str char))
      (get values char)
      )))



(defn suit [card]
  (let [[_ b] card]
    (str b)))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

;Helper function for some of the assignments:
(defn hand->val-frqs [hand]
  "Helper function that returns the ranks of the whole hand"
  (let[ints-hand (map rank hand)]
    (vals(frequencies ints-hand))))


;Assignments evaluating hands:
(defn pair? [hand]
  (>= (apply max (hand->val-frqs hand)) 2))



(defn three-of-a-kind? [hand]
  (>= (apply max (hand->val-frqs hand)) 3))



(defn four-of-a-kind? [hand]
  (>= (apply max (hand->val-frqs hand)) 4))


(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [frqs (set(hand->val-frqs hand))]
    (and
    (contains? frqs 2)
    (contains? frqs 3))))

(defn two-pairs? [hand]
  (let [frqs-of-frqs (frequencies(hand->val-frqs hand))]
    (and
    (= (get frqs-of-frqs 1) 1)
    (= (get frqs-of-frqs 2) 2))))

;trolling them assignmentz:
(defn straight? [hand]
  (if (pair? hand)
    false
    (let[ranks-hand (map rank hand)
         sorted-hand (sort ranks-hand)
         [a] sorted-hand
         min a
         [_ _ _ b] sorted-hand
         second-max b
         [_ _ _ _ c] sorted-hand
         max c]
        (cond
         (= (- max min) 4) true
         (= max 14) (= (- second-max 1) 4)
         :else false))))



(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

;Values "simple edition":
(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand)6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))

