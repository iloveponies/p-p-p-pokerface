(ns p-p-p-pokerface)

(def rank-values {\T 10, \J 11, \Q 12, \K 13, \A 14})

(comment
  (def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
  (def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
  (def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
  (def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
  (def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
  (def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
  (def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
  (def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
  (def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
  (def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
  (def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
  (def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
  (def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])
)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get rank-values rank))))

(defn suit [card]
  (let [[_ suit] card]
  (str suit)))


(defn n-of-a-kind [hand n]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (true? (some (fn [val] (== val n)) (vals freqs)))))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))


(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (frequencies suits)]
    (== (apply max (vals freqs)) 5)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq-vals (vals (frequencies ranks))]
    (or (four-of-a-kind? hand)
        (if (contains? (frequencies freq-vals) 2)
          (== (get (frequencies freq-vals) 2) 2)
          false))))

(defn straight? [hand]
  (let [ranks (sort(map rank hand))
        replaced-ranks (sort(replace {14 1} ranks))
        min-val (apply min ranks)
        max-val (apply max ranks)
        repl-max-val (apply max replaced-ranks)
        repl-min-val (apply min replaced-ranks)]
    (or (= ranks (range min-val (+ max-val 1)))
        (= replaced-ranks (range repl-min-val (+ repl-max-val 1))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
      categories (filter (fn [val] ((first val) hand)) checkers)
      scores (map (fn [pair] (second pair)) categories)]
  (apply max scores)))



