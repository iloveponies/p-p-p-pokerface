(ns p-p-p-pokerface)

(def rank-replacements
  {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get rank-replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn x-of-a-kind? [x hand]
  (let [ranks-in-hand (map rank hand)]
    (= x (apply max (vals (frequencies ranks-in-hand))))))

(defn pair? [hand]
  (x-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (x-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (x-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits-in-hand (map suit hand)]
    (= 5 (apply max (vals (frequencies suits-in-hand))))))

(defn full-house? [hand]
  (let [rank-frequencies (set (vals (frequencies (map rank hand))))]
    (and
      (contains? rank-frequencies 2)
      (contains? rank-frequencies 3))))

(defn two-pairs? [hand]
  (let [rank-frequencies (vals (frequencies (map rank hand)))
        two? (fn [x] (= 2 x))]
    (= 2 (count (filter two? rank-frequencies)))))

; uh
(defn straight? [hand]
  (let [rank-set (set (map rank hand))
        min-rank (apply min rank-set)]
    (or
      (= rank-set #{10 11 12 13 14})
      (= rank-set #{14 2 3 4 5})
      (and
        (contains? rank-set (+ 1 min-rank))
        (contains? rank-set (+ 2 min-rank))
        (contains? rank-set (+ 3 min-rank))
        (contains? rank-set (+ 4 min-rank))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(def value-tests
  [{:test high-card?, :value 0}
   {:test pair?, :value 1}
   {:test two-pairs?, :value 2}
   {:test three-of-a-kind?, :value 3}
   {:test straight?, :value 4}
   {:test flush?, :value 5}
   {:test full-house?, :value 6}
   {:test four-of-a-kind?, :value 7}
   {:test straight-flush?, :value 8}])

(defn value [hand]
  (let [if-test-ok-then-value (fn [value-test]
                                (if ((:test value-test) hand)
                                  (:value value-test)
                                  0))]
    (apply max (map if-test-ok-then-value value-tests))))
