(ns p-p-p-pokerface)

(defn suit [card]
  (str (second card)))

(def replacements {\T 10
                   \J 11
                   \Q 12
                   \K 13
                   \A 14})

(defn rank [card]
  (let [[rank suit] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn pair? [hand]
  "Is the hand a pair? Check if the counts of the rank frequencies are >= 1"
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
   (let [ranks (map rank hand)]
     (>= (apply max (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== 5 (apply max (vals (frequencies suits))))))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= [2 3] (sort (vals (frequencies ranks))))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (cond
     (= [2 2] (take-last 2 freqs)) true
     (== 4 (apply max freqs)) true
     :else false)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks-low (sort (replace {14 1} ranks))
        min-rank (apply min ranks)
        min-rank-low (apply min ranks-low)]
    (cond
     (= (range min-rank (+ min-rank 5)) ranks) true
     (= (range min-rank-low (+ min-rank-low 5)) ranks-low) true
     :else false)))

(defn straight-flush? [hand]
  (every? identity [(straight? hand) (flush? hand)]))

(defn high-card? [hand]
    true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        value (fn [[matcher value]] (if (matcher hand) value 0))]
    (apply max (map value checkers))))
