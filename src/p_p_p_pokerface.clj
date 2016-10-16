(ns p-p-p-pokerface)

(defn in? [collection element]
  (contains? (set collection) element))

(defn rank [card]
  (let [[rank _] card
        numeric? (Character/isDigit rank)
        special-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
   (if numeric?
     (Integer/valueOf (str rank))
     (get special-ranks rank))))

(defn suit [card]
  (let [[_ suit] card] (str suit)))

(defn n-of-a-kind? [hand n]
  (let [freqs (vals (frequencies (map rank hand)))]
   (in? freqs n)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
   (apply = suits)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))
        two-pairs [1 2 2]]
   (or (= freqs two-pairs) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [high-ace-straight (sort (map rank hand))
        low-ace-straight (sort (replace {14 1} high-ace-straight))
        lowest (first high-ace-straight)]
   (or
     (= (range 1 6) low-ace-straight)
     (= (range lowest (+ lowest 5)) high-ace-straight))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]    [pair? 1]
                   [two-pairs? 2]    [three-of-a-kind? 3]
                   [straight? 4]     [flush? 5]
                   [full-house? 6]   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check (fn [[checker _]] (checker hand))
        available-points (filter check checkers)
        points (map second available-points)]
    (apply max points)))
