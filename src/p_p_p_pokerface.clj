(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
    (let [[frst _] card
          card-rank (get replacements frst)]
    (Integer/valueOf (str (if card-rank card-rank frst)))))

(defn suit [card]
  (let [[_ scnd] card]
    (str scnd)))

(defn similar-rank [hand quantity]
  (= (apply max (vals (frequencies (map rank hand)))) quantity))

(defn pair? [hand]
  (similar-rank hand 2))

(defn three-of-a-kind? [hand]
  (similar-rank hand 3))

(defn four-of-a-kind? [hand]
  (similar-rank hand 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
 (= (sort (vals (frequencies (map rank hand)))) [2, 3]))

(defn two-pairs? [hand]
 (let [freq (frequencies (vals (frequencies (map rank hand))))
       actual-pairs-count (get freq 2)
       expected-pairs-count  (long 2)]
   (or (= expected-pairs-count actual-pairs-count) (contains? freq 4))))

(defn straight? [hand]
  (let [ranks (fn[hand] (map rank hand))
        ace-fourteen (apply min (ranks hand))
        ace-one (apply min (replace {14 1} (ranks hand)))]
      (or (= (sort (ranks hand))
             (sort (range ace-fourteen (+ ace-fourteen 5))))
          (= (sort (replace {14 1} (ranks hand)))
             (sort (range ace-one (+ ace-one 5)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [table #{[high-card? 0]
                [pair? 1]
                [two-pairs? 2]
                [three-of-a-kind? 3]
                [straight? 4]
                [flush? 5]
                [full-house? 6]
                [four-of-a-kind? 7]
                [straight-flush? 8]}]
   (apply max (map second (filter (fn [value] ((first value) hand)) table)))))

