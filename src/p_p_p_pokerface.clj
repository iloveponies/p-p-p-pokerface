(ns p-p-p-pokerface)


(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[fst _] card]

   (cond
     (Character/isDigit fst) (Integer/valueOf (str fst))
     :else                   (replacements fst)
  )))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)
  ))



(defn pair? [hand]
  (==
   (apply max
         (vals
          (frequencies
           (map rank hand))))
  2
  ))



(defn three-of-a-kind? [hand]
  (==
   (apply max
         (vals
          (frequencies
           (map rank hand))))
  3
  ))

(defn four-of-a-kind? [hand]
  (==
   (apply max
         (vals
          (frequencies
           (map rank hand))))
  4
  ))



(defn flush? [hand]
 (==
  (apply max
   (vals
    (frequencies
     (map suit hand))))
  5
  ))



(defn full-house? [hand]
  (=
  (sort(vals(frequencies(sort(map rank hand)))))
  [2 3]
  ))



(defn two-pairs? [hand]
  (cond
   (four-of-a-kind? hand) true
   :else                  (= [1 2 2]
                           (sort(vals(frequencies(map rank hand)))))
  ))


(defn straight? [hand]
  (let [the-hand (map rank hand)
        replaced (replace {14 1} the-hand)]
  (cond
  (contains? (set(seq the-hand)) 14) (or
                                       (=
                                          (sort(seq replaced))
                                          (range (apply min (seq replaced)) (+ 1 (apply max (seq replaced))))
                                       )

                                       (=
                                          (sort(seq the-hand))
                                          (range (apply min (seq the-hand)) (+ 1 (apply max (seq the-hand))))
                                        )
                                        )

  :else                               (=
                                         (sort(seq the-hand))
                                         (range (apply min (seq the-hand)) (+ 1 (apply max (seq the-hand))))
                                       )
  )))



(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand))
  )

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]

  (apply max (map second(filter #((first %) hand) checkers)))
  ))


