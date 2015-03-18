(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        number->rank
        {\A 14, \K 13
         \Q 12, \J 11
         \T 10}]
    (Integer/valueOf
      (if (Character/isDigit fst)
        (str fst)
        (str (number->rank fst))))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (> (apply max
            (vals
            (frequencies
            (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (== (apply max
             (vals
             (frequencies
             (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (== (apply max
             (vals
             (frequencies
             (map rank hand)))) 4))

(defn flush? [hand]
  (==
   (apply max
          (vals
           (frequencies
            (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort
      (vals
       (frequencies
        (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (let [pair-frequency
    (frequencies (vals (frequencies (map rank hand))))]
  (or (= (get pair-frequency 2) 2 ) (= (get pair-frequency 4) 1 ))))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        is-straight? (fn [sorted-hand]
                       (let [fst (first sorted-hand)]
                         (= sorted-hand (range fst (+ fst 5)))))]
    (or (is-straight? sorted-hand)
        (is-straight? (sort (replace {14 1} (map rank hand)))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        matching (fn [[k _v]] (k hand))
        points (map second (filter matching checkers))]
    (apply max points)))
