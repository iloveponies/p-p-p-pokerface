(ns p-p-p-pokerface)

(defn rank [[r _]]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (Integer/valueOf (str (replacements r))))))

(defn suit [[_ s]]
  (str s))

(defn pair? [hand]
  (let [ranks (map rank hand)
        frequencies (vals (frequencies ranks))
        count-pairs (count (filter (fn [x] (>= x 2)) frequencies))]
    (>= count-pairs 1)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        frequencies (vals (frequencies ranks))
        count-threes (count (filter (fn [x] (>= x 3)) frequencies))]
    (>= count-threes 1)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        frequencies (vals (frequencies ranks))
        count-fours (count (filter (fn [x] (>= x 4)) frequencies))]
    (>= count-fours 1)))

(defn flush? [hand]
  (let [suits (map suit hand)
        frequencies (vals (frequencies suits))
        count-fives (count (filter (fn [x] (== x 5)) frequencies))]
    (>= count-fives 1)))

(defn full-house? [hand]
  (let [suits (map rank hand)
        frequencies (vals (frequencies suits))
        count-pairs (count (filter (fn [x] (== x 2)) frequencies))
        count-threes (count (filter (fn [x] (== x 3)) frequencies))]
    (and (== count-pairs 1) (== count-threes 1))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        frequencies (vals (frequencies ranks))
        count-pairs (count (filter (fn [x] (>= x 2)) frequencies))]
    (>= count-pairs 2)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        ranks-with-low-ace (replace {14 1} ranks)
        sorted-ranks-with-low-ace (sort ranks-with-low-ace)
        min (first sorted-ranks)
        min-with-low-ace (first sorted-ranks-with-low-ace)
        straight (range min (+ min 5))
        straight-with-low-ace (range min-with-low-ace (+ min-with-low-ace 5))]
    (or
     (= sorted-ranks straight)
     (= sorted-ranks-with-low-ace straight-with-low-ace))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hand-checker (fn [[func val]] [val (func hand)])
        scores (map hand-checker checkers)
        won-scores (map (fn [[score _]] score) (filter (fn [[_ won]] won) scores))]
    (apply max won-scores)))
