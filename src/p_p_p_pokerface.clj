(ns p-p-p-pokerface)

(def rank->num {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn card [a-card]
  a-card)

(defn hand [a-hand]
  a-hand)

(defn rank [card]
  (let [[rank _] card
        digit? (fn [x] (Character/isDigit x))
        int-value (fn [y] (Integer/valueOf (str y)))] ; valueOf needs string as input
    (if (digit? rank)
      (int-value rank)
      (int-value (rank->num rank)))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn ranks->frequency [hand]
  (let [rank-seq (map rank hand)]
    (frequencies rank-seq)))

(defn ranks->freq-vals [hand]
  (vals (ranks->frequency hand)))

(defn pair? [hand]
  (let [max-count (apply 
                   max 
                   (ranks->freq-vals hand))]

    (if (> max-count 1) 
      true 
      false)))

(defn three-of-a-kind? [hand]
  (let [freq-vals (ranks->freq-vals hand)
        three-of-a-kind (filter (fn [x] (= x 3)) freq-vals)]
    (if (empty? three-of-a-kind)
      false
      true)))

(defn four-of-a-kind? [hand]
  (let [freq-vals (ranks->freq-vals hand)
        four-of-a-kind (filter (fn [x] (= x 4)) freq-vals)]
    (if (empty? four-of-a-kind)
      false
      true)))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-freq (frequencies suits)
        vals (vals suit-freq)
        seq-size (count vals)]
    (= seq-size 1)))

(defn full-house? [hand]
  (let [same-rank-freq (ranks->freq-vals hand)]
    (= (sort same-rank-freq) (range 2 4))))

(defn two-pairs? [hand]
  (let [distinct-pairs (= (sort (ranks->freq-vals hand)) [1 2 2])
        same-pairs (= (sort (ranks->freq-vals hand)) [1 4])]
    (or same-pairs distinct-pairs)))

;; (defn straight? [hand]
;;   (let [sorted-ranks (sort (map rank hand))
;;         first-rank (first sorted-ranks)
;;         fixed-ranks (if (= first-rank 2) 
;;                       (sort (replace {14 1} sorted-ranks))
;;                       sorted-ranks)
;;         first-rank (first fixed-ranks)
;;         last-rank (+ first-rank 5)] 

;;     (if (= fixed-ranks (range first-rank last-rank)) 
;;       true 
;;       false)))

(defn straight? [hand]
  (let [ranks (set (map rank hand))
        corrected-ranks (if (contains? ranks 2 ) ; change A -> 14 to A -> 1 if there's 2 in hand 
                          (sort (replace {14 1} ranks))
                          (seq ranks))
        first-rank (first corrected-ranks)
        last-rank (+ first-rank 5)] 
    
    (if (= corrected-ranks (range first-rank last-rank)) 
      true 
      false)))

(defn high-card? [hand] 
  true)

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true 
    false))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        is-hand? (fn [fn-val] ((first fn-val) hand))]
    (apply max (map second (filter is-hand? checkers)))))

