(ns p-p-p-pokerface)

(defn rank [card]
  (let[[r _] card
       mapper {\A 14, \K 13, \Q 12, \J 11, \T 10}]
    (if (Character/isDigit r) (Integer/valueOf (str r)) (get mapper r))))

(defn suit [card]
  (let[[rank suit] card]
    (str suit)))

(defn ranks [hand]
  (map rank hand))

(defn suits [hand]
  (map suit hand))

(defn vcontains? [my-vector x]
  (contains? (into #{} my-vector) x))

(defn to-freq-values [my-vector]
  (vals (frequencies my-vector)))

(defn high-card? [card]
  (= 1 1))

(defn pair? [hand]
  (vcontains? (vals (frequencies (ranks hand))) 2))

(defn three-of-a-kind? [hand]
  (vcontains? (vals (frequencies (ranks hand))) 3))

(defn four-of-a-kind? [hand]
  (vcontains? (vals (frequencies (ranks hand))) 4))

(defn flush? [hand]
  (vcontains? (vals (frequencies (suits hand))) 5))

(defn full-house? [hand]
  (and (vcontains? (vals (frequencies (ranks hand))) 2) (vcontains? (vals (frequencies (ranks hand))) 3)))

(defn two-pairs? [hand]
  (= (get (frequencies (to-freq-values (ranks hand))) 2) 2))

(defn straight? [hand]
  (let [hi-ace-hand (sort (ranks hand))
        lo-ace-hand (sort (replace {14 1} (ranks hand)))
        fst-lo (first lo-ace-hand)
        fst-hi (first hi-ace-hand)
        expected-straight-lo (range fst-lo (+ fst-lo 5))
        expected-straight-hi (range fst-hi (+ fst-hi 5))]
    (or (= expected-straight-lo lo-ace-hand) (= expected-straight-hi hi-ace-hand))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
      check (fn [entry] (if ((first entry) hand) (second entry) nil))
      non-null? (fn [x] (not (= x nil)))]
  (apply max (filter non-null? (map check checkers)))))
