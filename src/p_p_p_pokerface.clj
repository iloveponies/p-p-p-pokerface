(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-char (first card)]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (let
        [ replacements { \T 10
                         \J 11
                         \Q 12
                         \K 13
                         \A 14}]
        (replacements rank-char)))))

(defn suit [card]
  (str (second card)))

(defn n-of-a-kind? [hand n]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (>= (apply max freqs)
        n)))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= 1 (count (set suits)))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (= freqs '(2 3))))

(defn two-pairs? [hand]
  (or
    (four-of-a-kind? hand)
    (let [ranks (map rank hand)
          sfreqs (sort >= (vals (frequencies ranks)))]
      (>= (second sfreqs) 2))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        min-rank (apply min ranks)
        max-rank (apply max ranks)]
    (and
      (= 5 (count (set ranks)))
      (or
        (= (- max-rank min-rank) 4)
        (= (set ranks) #{14 2 3 4 5})))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                    [two-pairs? 2]  [three-of-a-kind? 3]
                    [straight? 4]   [flush? 5]
                    [full-house? 6] [four-of-a-kind? 7]
                    [straight-flush? 8]}]
     (apply max (map (fn [[f hv]] (if (f hand) hv 0)) checkers))))
