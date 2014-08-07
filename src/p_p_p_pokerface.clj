(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank_char _] card
        nobility {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank_char)
      (Integer/valueOf (str rank_char))
      (get nobility  rank_char))))

(defn suit [card]
  (let [[_  suit_char] card]
    (str suit_char)))
(defn number-of-type [type hand]
                (vals (frequencies (map type hand))))
(defn same-type? [type hand count]
  (contains? (set (number-of-type type hand)) count))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (same-type? rank hand 2))

(defn three-of-a-kind? [hand]
  (same-type? rank hand 3))

(defn four-of-a-kind? [hand]
  (same-type? rank hand 4))

(defn flush? [hand]
  (same-type? suit hand 5))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
   (= 2 (count (filter #(= % 2) (number-of-type rank hand))))
   (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        match? (fn [ranks]
                 (let [sorted-hand (sort ranks)
                       start (apply min sorted-hand)
                       end (+ 1 (apply max sorted-hand))]
                  (= sorted-hand
                     (range start end))))]
    (or (match? ranks)
        (match? (replace {14 1} ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        hand? #((get % 0) hand)
        get-value #(get % 1)]
    (apply max (map get-value (filter hand? checkers)))))
