(ns p-p-p-pokerface)

(defn card [a-card] a-card)

(defn hand [a-hand] a-hand)

(defn rank [card]
  (let [repl {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [x _] card
        pint (fn [x] (Integer/parseInt (str x)))]
    (if (Character/isDigit x) (pint x)
          (pint (repl x)))))


(defn suit [card]
  (let [[x y] card]
    (str y)))

(defn highestFq [hand func]
  (let [elems (map func hand)
        values (vals (frequencies elems))
        mx (apply max values)]
    mx))

(defn high-card? [hand] true)

(defn pair? [hand]
  (if (>= (highestFq hand rank) 2) true false))

(defn three-of-a-kind? [hand]
  (if (>= (highestFq hand rank) 3) true false))

(defn four-of-a-kind? [hand]
  (if (>= (highestFq hand rank) 4) true false))

(defn flush? [hand]
  (if (>= (highestFq hand suit) 5) true false))

(defn sortedFq [hand] (sort (vals (frequencies (map rank hand)))))

(defn full-house? [hand]
    (= [2 3] (sortedFq hand)))

(defn two-pairs? [hand]
  (let [hnd (rest (sortedFq hand))]
    (or
      (= hnd [2 2])
      (= hnd [4]))))

(defn straight? [hand]
  (let [sHandH (sort (map rank hand))
        sHandL (sort (replace {14 1} sHandH))
        rng (fn [x] (range (first x) (+ 1 (last x))))]
    (or
     (= sHandH (rng sHandH))
     (= sHandL (rng sHandL)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        potHands (filter (fn [x] ((first x) hand)) checkers)
        highHand (apply max (map second potHands))]
    highHand))

