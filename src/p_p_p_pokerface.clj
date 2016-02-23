(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacements rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn- hand-rank-frequencies [hand]
  (set (vals (frequencies (map rank hand)))))

(defn- hand-has-frequency [hand freq]
  (contains? (hand-rank-frequencies hand) freq))

(defn pair? [hand]
  (hand-has-frequency hand 2))

(defn three-of-a-kind? [hand]
  (hand-has-frequency hand 3))

(defn four-of-a-kind? [hand]
  (hand-has-frequency hand 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (hand-has-frequency hand 2)
       (hand-has-frequency hand 3)))

(defn two-pairs? [hand]
  (== (count
        (filter (fn [x] (let [[k f] x] (== f 2)))
          (frequencies (map rank hand))))
      2))

(defn- sorted-is-straight? [hand]
  (if (empty? hand)
    true
    (let [hfirst (first hand)
          hrest (rest hand)]
      (if (or (empty? hrest)
              (== (+ 1 (first hand)) (second hand)))
        (sorted-is-straight? (rest hand))
        false))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        sorted-ranks-low-ace (sort (replace {14 1} ranks))]
    (or (sorted-is-straight? sorted-ranks)
        (sorted-is-straight? sorted-ranks-low-ace))))

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
                   [straight-flush? 8]}]
    (apply max
      (map second
        (filter (fn [x] (let [[f v] x] (f hand)))
        checkers)))))
