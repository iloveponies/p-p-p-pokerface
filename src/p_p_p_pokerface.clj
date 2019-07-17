(ns p-p-p-pokerface)

; test hands
;(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
;(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
;(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
;(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
;(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
;(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
;(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
;(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
;(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
;(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
;(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
;(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
;(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])
;


(def rank-table {\2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank suit] card]
    (get rank-table rank)))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [rank-count (reverse (sort (vals (frequencies (map rank hand)))))]
    (and (= 3 (first rank-count))
         (= 2 (second rank-count)))))

(defn two-pairs? [hand]
  (let [rank-count (reverse (sort (vals (frequencies (map rank hand)))))]
    (or
      (and (= 2 (first rank-count))
           (= 2 (second rank-count)))
      (= 4 (first rank-count)))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks-aces-low (sort (replace {14 1} ranks))
        start-rank (apply min ranks)
        comp-straight (range start-rank (+ start-rank 5))
        comp-straight-low (range 1 6)]
    (or (= ranks comp-straight)
        (= ranks-aces-low comp-straight-low))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hands (map (fn [x] (if ((first x) hand) (second x) nil)) checkers)
        values (filter (fn [x] (not (nil? x))) hands)]
    (apply max values)))
