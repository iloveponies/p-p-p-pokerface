(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [first-letter (first card)]
    (if (Character/isDigit first-letter)
      (Integer/valueOf (str first-letter))
      (replacements first-letter))))

(defn suit [card]
  (str (second card)))

(defn freqs [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (boolean (some (fn [x] (== x 2))
                 (freqs hand))))

(defn three-of-a-kind? [hand]
  (boolean (some (fn [x] (== x 3))
                 (freqs hand))))

(defn four-of-a-kind? [hand]
  (boolean (some (fn [x] (== x 4))
                 (freqs hand))))

(defn flush? [hand]
  (let [suits-set (set (map suit hand))]
    (== (count suits-set) 1)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (== (count (filter (fn [x] (== x 2))
                     (freqs hand)))
      2))

(defn straight? [hand]
  (let [ranks (map rank hand)
        straight-internal? (fn [rs]
                             (let [lower-bound (apply min rs)
                                   range-to-match (range lower-bound (+ lower-bound 5))]
                               (= (sort rs)
                                  range-to-match)))]
    (or (straight-internal? ranks)
        (straight-internal? (replace {14 1} ranks)))))

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
        filtered-checkers (filter (fn [checker] ((first checker) hand))
                                  checkers)
        values (map second filtered-checkers)]
    (apply max values)))
