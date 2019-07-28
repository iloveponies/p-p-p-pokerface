(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        high-ranks {\T 10 \J 11
                    \Q 12 \K 13
                    \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get high-ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        max-occurence (apply max freqs)]
    (>= max-occurence 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        max-occurence (apply max freqs)]
    (>= max-occurence 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        max-occurence (apply max freqs)]
    (>= max-occurence 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))
        max-occurence (apply max freqs)]
    (>= max-occurence 5)))

(defn full-house? [hand]
   (let [ranks (map rank hand)
         freqs (vals (frequencies ranks))
         sorted-freqs (sort freqs)]
     (= sorted-freqs '(2 3))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
         freqs (vals (frequencies ranks))
         sorted-freqs (sort freqs)]
    (or (four-of-a-kind? hand)
        (full-house? hand)
        (= sorted-freqs '(1 2 2)))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        ace-adjusted (sort (replace {14 1} ranks))
        first-of-sorted (first sorted-ranks)
        first-of-adjusted (first ace-adjusted)]
    (or (= (range first-of-sorted (+ first-of-sorted 5))
           sorted-ranks)
        (= (range first-of-adjusted (+ first-of-adjusted 5))
           ace-adjusted))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        qualifies? (fn [checker] ((first checker) hand))
        qualfied-hands (filter qualifies? checkers)
        qualifies-points (map second qualfied-hands)]
    (apply max qualifies-points)))
