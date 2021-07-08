(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [rank-char (first card)]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (replacements rank-char))))

(defn suit [card]
 (let [[_ suit] card]
   (str suit)))

(defn appearances [func coll]
  (let [instances (map func coll)]
    (vals (frequencies instances))))

(defn max-appearances [func coll]
  (apply max (appearances func coll)))

(defn high-card? [hand]
    true) ; All hands have a high card.

(defn pair? [hand]
  (== (max-appearances rank hand) 2))

(defn three-of-a-kind? [hand]
  (== (max-appearances rank hand) 3))

(defn four-of-a-kind? [hand]
  (== (max-appearances rank hand) 4))

(defn flush? [hand]
  (== (max-appearances suit hand) 5))

(defn full-house? [hand]
  (let [rank-count (sort (appearances rank hand))]
    (= rank-count [2 3])))

(defn two-pairs? [hand]
  (if (four-of-a-kind? hand) true
    (let [pair-count ((frequencies (appearances rank hand)) 2)]
      (if (not (nil? pair-count))
        (if (== pair-count 2) true false)
        false))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        min-rank (apply min ranks)]
    (if (= (sort ranks) (range min-rank (+ min-rank 5)))
      true
      (if (= (sort (replace {14 1} ranks)) (range 1 6)) true false))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (let [hand-values 
          (map (fn [checker] (second checker))
               (filter (fn [checker] 
                         ((first checker) hand))
                 checkers))]
      (apply max hand-values))))
