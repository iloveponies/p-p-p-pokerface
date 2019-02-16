(ns p-p-p-pokerface)

(def two-digit-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank-character _] card]
    (if (Character/isDigit rank-character)
      (Integer/valueOf (str rank-character)) 
      (two-digit-ranks rank-character))))

(defn suit [card]
  (let [[_ suit-character] card]
    (str suit-character)))

(defn ranks [hand]
  (map rank hand))

(defn rank-freq-values [hand] 
  (vals (frequencies (ranks hand))))

(defn pair? [hand]
  (= [1 1 1 2] (sort (rank-freq-values hand))))

(defn three-of-a-kind? [hand]
  (= [1 1 3] (sort (rank-freq-values hand))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (rank-freq-values hand))))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (= [2 3] (sort (rank-freq-values hand))))

(defn two-pairs? [hand]
   (= [1 2 2] (sort (rank-freq-values hand))))

(defn straight? [hand]
  (letfn [(is-straight? [rankss]
            (= (sort rankss) (range (apply min rankss) (inc (apply max rankss)))))]
    (or (let [low-aces-ranks (replace {14 1} (ranks hand))]
          (is-straight? low-aces-ranks))
        (is-straight? (ranks hand)))))

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
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
