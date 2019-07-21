(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank (first card)
        rank-map {\A 14
                  \K 13
                  \Q 12
                  \J 11
                  \T 10}]
    (if (contains? rank-map rank)
      (get rank-map rank)
      (Integer. (str rank)))))

(defn suit [card]
  (str (second card)))

(defn- n-of-rank [hand n]
  (let [rank-freq (frequencies (map rank hand))]
    (boolean
     (some #{n} (vals rank-freq)))))

(defn pair? [hand]
  (n-of-rank hand 2))

(defn three-of-a-kind? [hand]
  (n-of-rank hand 3))

(defn four-of-a-kind? [hand]
  (n-of-rank hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (every? #(= (first suits) %) suits)))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (let [rank-freq (frequencies (map rank hand))]
    (= 2 (count (filter #(= 2 %) (vals rank-freq))))))

(defn straight? [hand]
  (let [sorted (sort > (map rank hand))]
    (or (= sorted [14 5 4 3 2])
        (every? #(= % 1) (map #(- (first %) (second %))
                              (partition 2 1 sorted))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand)  8
   (four-of-a-kind? hand)  7
   (full-house? hand)      6
   (flush? hand)           5
   (straight? hand)        4
   (three-of-a-kind? hand) 3
   (two-pairs? hand)       2
   (pair? hand)            1
   :else                   0))

(comment
  (do
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
    (def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])))
