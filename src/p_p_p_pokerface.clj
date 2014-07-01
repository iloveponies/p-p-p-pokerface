(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card]
    (cond (Character/isDigit rnk)  (Integer/valueOf (str rnk))
          (= \T rnk) 10
          (= \A rnk) 14
          (= \J rnk) 11
          (= \Q rnk) 12
          (= \K rnk) 13
          :else nil    )))


(defn suit [card]
  (let [[_ su] card] (str su) ))


(defn pair? [hand]
  (let [freqs (apply max (vals (frequencies (map rank hand))))]
    (= freqs 2)
    ))

(defn three-of-a-kind? [hand]
  (let [freqs (apply max (vals (frequencies (map rank hand))))]
    (= freqs 3)
    ))


(defn four-of-a-kind? [hand]
 (let [freqs (apply max (vals (frequencies (map rank hand))))]
    (= freqs 4)
    ))

(defn flush? [hand]

   (let [freqs (apply max (vals (frequencies (map suit hand))))]
    (= freqs 5)
    ))


(defn full-house? [hand]
  (let [the-hand (sort (vals (frequencies (map rank hand))))
        test-hand (range 2 4)]
    (= the-hand test-hand)
   ))


(defn two-pairs? [hand]
  (let [the-hand (sort (vals (frequencies (map rank hand))))]
    (or (= the-hand (seq [1 2 2]))
        (four-of-a-kind? hand)
        )))


 (defn straight? [hand]
  (let [hand-ranks (sort (map rank hand))
        lowest (first hand-ranks)
        test-hand (range lowest (+ 5 lowest))
        altered-hand (sort (replace {14 1} hand-ranks ))
        other-lowest (first altered-hand)
        other-test-hand (range other-lowest (+ 5 other-lowest))
        ]
    (or (= hand-ranks test-hand)
        (= altered-hand other-test-hand))
    ))


(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.


(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
         check (fn [the-checker] (let [method (first the-checker) score (second the-checker)]
                                   (if (method hand) score)))]

     (apply max (filter number? (map check checkers)))))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
