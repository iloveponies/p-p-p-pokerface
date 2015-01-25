(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        face-cards-values {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if
      (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get face-cards-values rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn card-frequencies [f hand]
  (vals (frequencies (map f hand))))

(defn pair? [hand]
  (let [two? (fn [f] (= (apply f (card-frequencies rank hand)) 2))]
    (or (two? max) (two? min))))

(defn three-of-a-kind? [hand]
  (= (apply max (card-frequencies rank hand)) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (card-frequencies rank hand)) 4))

(defn flush? [hand]
  (= (apply max (card-frequencies suit hand)) 5))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (or
    (= (get (frequencies (card-frequencies rank hand)) 2) 2)
    (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [highAceHand (map rank hand)
        lowAceHand (replace {14 1} highAceHand)
        check (fn [coll] (let [sorted (sort coll)
                               lowRank (first sorted)
                               straight (range lowRank (+ lowRank 5))]
                           (= sorted straight)))]
    (or (check highAceHand) (check lowAceHand))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[pair? 1] [two-pairs? 2] [three-of-a-kind? 3] [straight? 4]
                   [flush? 5] [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}
        check (fn [checker] (let [[function value] checker] (if (function hand) value 0)))]
    (apply max (map check checkers))))