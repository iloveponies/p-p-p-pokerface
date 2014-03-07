(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank-char _] card
        rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (rank-map rank-char))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn- hand-frequency [f hand]
  (vals (frequencies (map f hand))))

(defn- max-frequency [f hand]
  (apply max (hand-frequency f hand)))

(defn pair? [hand]
  (= 2 (max-frequency rank hand)))

(defn three-of-a-kind? [hand]
  (= 3 (max-frequency rank hand)))

(defn four-of-a-kind? [hand]
  (= 4 (max-frequency rank hand)))

(defn flush? [hand]
  (= 5 (max-frequency suit hand)))

(defn full-house? [hand]
  (= [2 3] (sort (hand-frequency rank hand))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (hand-frequency rank hand))))

(defn- straight-range [ranks]
  (range (first ranks) (inc (last ranks))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks-with-ace-as-1 (sort (replace {14 1} ranks))]
    (or (= ranks (straight-range ranks))
        (= ranks-with-ace-as-1 (straight-range ranks-with-ace-as-1)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
        matches (filter (fn [checker] ((first checker) hand)) checkers)
        values (map second matches)]
    (apply max values)))
