(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst snd] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn ranks [hand]
  (map rank hand))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn max-rank-freq [hand]
  (let [rank-freqs     (frequencies (ranks hand))
        rank-freq-vals (vals rank-freqs)
        max-rank-freq  (apply max rank-freq-vals) ]
    max-rank-freq))

(defn max-suit-freq [hand]
  (let [suits          (map suit hand)
        suit-freqs     (frequencies suits)
        suit-freq-vals (vals suit-freqs)
        max-suit-freq  (apply max suit-freq-vals)]
    max-suit-freq))

(defn high-card-rank [hand]
  (apply max (ranks hand)))

(defn low-card-rank [hand]
  (apply min (ranks hand)))

(defn pair? [hand]
  (== (max-rank-freq hand) 2))

(defn three-of-a-kind? [hand]
  (== (max-rank-freq hand) 3))

(defn four-of-a-kind? [hand]
  (== (max-rank-freq hand) 4))

(defn strict-monotonic? [hand]
  (apply < (sort (ranks hand))))

(defn spread [hand]
  (- (high-card-rank hand) (low-card-rank hand)))

(defn flush? [hand]
    (and
     (== (max-suit-freq hand) 5)
     (> (spread hand) 4)
     (strict-monotonic? hand)))

(defn sorted-freqs [hand]
  (let [rank-freqs (frequencies (ranks hand))
        rank-freq-vals (vals rank-freqs)
        sorted-freqs (sort rank-freq-vals)]
    sorted-freqs))

(defn full-house? [hand]
  (= (sorted-freqs hand) [2 3]))

(defn two-pairs? [hand]
  (or
   (= (sorted-freqs hand) [1 2 2])
   (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ace-replace (replace {14 1} (ranks hand))
        max-ace-replace (apply max ace-replace)
        min-ace-replace (apply min ace-replace)
        spread-ace-replace (- max-ace-replace min-ace-replace)
        not-all-same-suit? (< (max-suit-freq hand) 5)]
    (and
     not-all-same-suit?
     (strict-monotonic? hand)
     (or
      (< (spread hand) 5)
      (< spread-ace-replace 5)))))

(defn straight-flush? [hand]
  (let [ace-replace (replace {14 1} (ranks hand))
        max-ace-replace (apply max ace-replace)
        min-ace-replace (apply min ace-replace)
        spread-ace-replace (- max-ace-replace min-ace-replace)
        all-same-suit? (== (max-suit-freq hand) 5)]
    (and
     all-same-suit?
     (strict-monotonic? hand)
     (or
      (< (spread hand) 5)
      (< spread-ace-replace 5)))))

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
                   [straight-flush? 8]}
        checked-value (fn [checker] (if ((first checker) hand) (second checker)))
        value-map (map checked-value checkers)
        values (filter boolean value-map)
        return-value (apply max values)]
    return-value))
