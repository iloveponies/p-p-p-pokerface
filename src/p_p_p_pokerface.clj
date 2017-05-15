(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn ranks-in-hand [hand]
  (let [get-rank (fn [x] (rank x))]
    (map get-rank hand)))

(defn suits-in-hand [hand]
  (let [get-suit (fn [x] (suit x))]
    (map get-suit hand)))

(defn frequencies-in-hand [hand]
  (vals (frequencies (ranks-in-hand hand))))

(defn max-number-of-ranks [hand]
  (apply max (frequencies-in-hand hand)))

(defn pair? [hand]
  (= 2 (max-number-of-ranks hand)))

(defn three-of-a-kind? [hand]
  (= 3 (max-number-of-ranks hand)))

(defn four-of-a-kind? [hand]
  (= 4 (max-number-of-ranks hand)))

(defn flush? [hand]
  (let [suits (suits-in-hand hand)]
    (apply = suits)))

(defn full-house? [hand]
  (= (seq[3 2]) (frequencies-in-hand hand)))

(defn two-pairs? [hand]
  (let [freqs (frequencies-in-hand hand)]
    (or
      (= (seq[2 2 1]) freqs)
      (= (seq[4 1]) freqs))))

(defn straight? [hand]
  (let [sorted-ranks (sort (ranks-in-hand hand))]
    (let [smallest-rank (first sorted-ranks)]
      (let [expected-range (range smallest-rank (+ smallest-rank 5))]
        (let [expected-range-with-low-ace [2 3 4 5 14]]
          (or
            (= expected-range-with-low-ace sorted-ranks)
            (= expected-range sorted-ranks)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (let [points-with-nil (map (fn [checker] (when ((first checker) hand) (second checker))) checkers)]
      (let [points (filter (fn[x](boolean x)) points-with-nil)]
        (apply max points)))))
