(ns p-p-p-pokerface)

(defn hand
  "hand"
  [a-hand]
  a-hand)

(defn card
  "card"
  [a-card]
  a-card)

(defn rank [card]
  (let [[r _] card
        c-map {\T 10
               \J 11
               \Q 12
               \K 13
               \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (c-map r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn contains-n [hand n-same]
  (let [freq (frequencies (map rank hand))]
    (true? (some #(= % n-same) (vals freq)))))

(defn pair? [hand]
  (contains-n hand 2))

(defn three-of-a-kind? [hand]
  (contains-n hand 3))

(defn four-of-a-kind? [hand]
  (contains-n hand 4))

(defn flush? [hand]
  (let [freq (frequencies (map suit hand))]
    (= (count freq) 1)))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freq (frequencies (map rank hand))]
    (= 2 (count (filter #(= % 2) (vals freq))))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        freq (frequencies sorted-ranks)
        lowest (first sorted-ranks)]
    (and
      (= 5 (count freq))                                    ;; all cards have to be different
      (or
        (= (keys freq) (range lowest (+ lowest 5)))         ;; cards are 4 apart thus a straight as all are diff
        (= (keys freq) '(2 3 4 5 14))))))                   ;; Ace low straight

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
