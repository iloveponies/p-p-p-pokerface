(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (cond (Character/isDigit fst) (Integer/valueOf (str fst))
          :else (get {\A 14, \K 13, \Q 12, \J 11, \T 10} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn cards-of-same-rank? [hand n]
  (if (some (fn [rank] (= rank n)) (vals (frequencies (map rank hand))))
    true
    false))

(defn pair? [hand]
  (cards-of-same-rank? hand 2))

(defn three-of-a-kind? [hand]
  (cards-of-same-rank? hand 3))

(defn four-of-a-kind? [hand]
  (cards-of-same-rank? hand 4))

(defn flush? [hand]
  (if (apply = (map suit hand))
    true
    false))

(defn full-house? [hand]
  (and (cards-of-same-rank? hand 2)
       (cards-of-same-rank? hand 3)))

(defn two-pairs? [hand]
  (let [rank-frequency-values (vals (frequencies (map rank hand)))]
    (= 2 (get (frequencies rank-frequency-values) 2))))

(defn straight? [hand]
  (let [hand-ranks (map rank hand)
        hand-ranks-set (set hand-ranks)
        lowest-card (first (sort hand-ranks))
        straight-set (set (range lowest-card (+ lowest-card 5)))]
    (if (and (= (count hand-ranks-set) 5)
             (some true? [(empty? (clojure.set/difference hand-ranks-set straight-set))
                          (empty? (clojure.set/difference hand-ranks-set #{14 2 3 4 5}))
                          (empty? (clojure.set/difference hand-ranks-set #{14 13 12 11 10}))]))
      true
      false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))

