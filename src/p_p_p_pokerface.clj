(ns p-p-p-pokerface)

;;GJG
(def face-cards {"T" 10, "J" 11, "Q" 12, "K" 13, "A" 14})
(def face-cards-ace-low {"T" 10, "J" 11, "Q" 12, "K" 13, "A" 1})

(defn rank
  ([card] (rank card face-cards))
  ([card fcards]
     (let [[r _] card]
       (if (Character/isDigit r)
         (Integer/valueOf (str r))
         (get fcards (str r))))))

;;GJG
(defn ranks
  "Return unsorted list of ranks in a hand"
  ([hand] (ranks hand face-cards))
  ([hand fcards]
     (map #(rank % fcards) hand)))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

;;GJG
(defn suits
  "Return unsorted list of suits in a hand"
  [hand]
  (map #(suit %) hand))

(defn pair? [hand]
  (let [my-ranks (ranks hand)]
    (>= (apply max (vals (frequencies my-ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [my-ranks (ranks hand)]
    (>= (apply max (vals (frequencies my-ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [my-ranks (ranks hand)]
    (>= (apply max (vals (frequencies my-ranks))) 4)))

(defn flush? [hand]
  ;; treat it as 
  (let [suits (suits hand)]
    (>= (apply max (vals (frequencies suits))) 5)))


(defn full-house? [hand]
  (let [my-ranks (ranks hand)]
    (= '(2 3) (sort (vals (frequencies my-ranks))))))

(defn two-pairs? [hand]
  (let [my-ranks (ranks hand)
        pairings (filter #(>= % 2) (vals (frequencies my-ranks)))]
    (or (>= (count pairings) 2)
        (and (not (empty? pairings))
             (>= (first pairings) 4)))))


(defn straight? [hand]
  ;; ace can be considered either a 1 or a 14 as rank
  (let [my-ranks (ranks hand)
        sorted-ranks (sort my-ranks)
        lowest-card (first sorted-ranks)
        sorted-ranks-ace-low (sort (ranks hand face-cards-ace-low))]
    (or (= sorted-ranks (range (first sorted-ranks) (+ 5 (first sorted-ranks))))
        (= sorted-ranks-ace-low (range (first sorted-ranks-ace-low) (+ 5 (first sorted-ranks-ace-low)))))))


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

;;GJG
(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value
  "Hand	Value
  High card (nothing)	0
  Pair	1
  Two pairs	2
  Three of a kind	3
  Straight	4
  Flush	5
  Full house	6
  Four of a kind	7
  Straight flush	8"
  [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        ]
    (apply max
           (filter #(not (nil? %))
                   (map #(if ((first %) hand) (second %)) checkers)))))
