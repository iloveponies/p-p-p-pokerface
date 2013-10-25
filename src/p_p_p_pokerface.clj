(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        rep {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get rep fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn multiple-tester [multiples]
  (fn [hand]
    (let [arvot (map rank hand)
          jakauma (vals (frequencies arvot))
          samat (filter (fn [x] (> x multiples)) jakauma)]
      (not (empty? samat)))))

(def pair? (multiple-tester 1))

(def three-of-a-kind? (multiple-tester 2))

(def four-of-a-kind? (multiple-tester 3))

(defn flush? [hand]
  (let [arvot (map suit hand)
        maat (set arvot)]
    (== 1 (count maat))))

(defn full-house? [hand]
  (and (two-pairs? hand) (three-of-a-kind? hand) (not (four-of-a-kind? hand))))

(defn two-pairs? [hand]
  (let [arvot (map rank hand)
          jakauma (vals (frequencies arvot))
          samat (filter (fn [x] (> x 1)) jakauma)]
      (or (>= (count samat) 2)
          (and (not (empty? samat)) (== 4 (first samat))))))

(defn straight? [hand]
  (let [arvot (seq (sort (map rank hand)))
        vertailu1 (range (first arvot) (+ 1 (last arvot)))
        vertailu2 (sort (cons 14 (range 2 6)))]
    (or (= arvot vertailu1) (= arvot vertailu2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value-r [hand kadet]
  (if (empty? kadet)
    0
    (if ((first kadet) hand)
      (count kadet)
      (value-r hand (rest kadet)))))

(defn value [hand]
  (value-r hand [straight-flush? four-of-a-kind? full-house?
                 flush? straight? three-of-a-kind? two-pairs? pair?]))



