(ns p-p-p-pokerface)

(defn rank
  "Takes a card and returns its rank as integer"
  [[rank _]]
  (let [pic-vals {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (pic-vals rank))))

(defn suit
  "Takes a card and returns its suit"
  [[_ suit]]
  (str suit))

(defn rank-frequencies
  "{2 2, 4 1, 5 1, 7 1} means two twos one four one five one seven"
  [hand]
  (frequencies (map rank hand)))

(defn suit-frequencies
  "{H 2, C 1, D 1, S 1} means two hearts one club one diamong one spade"
  [hand]
  (frequencies (map suit hand)))

(defn hand-structure
  "Given a rank or suit frequency this returns a map representing the structure of a hand. {2 1, 1 3} means one pair and 3 singles of a rank or suit."
  [freqs]
  (frequencies (vals freqs)))

(defn x-of-a-kind [freq x]
  (contains? (hand-structure freq) x))

(defn x-of-a-kind-num [freq x]
  ((hand-structure freq) x))

(defn pair? [hand]
  (x-of-a-kind (rank-frequencies hand) 2))

(defn three-of-a-kind? [hand]
  (x-of-a-kind (rank-frequencies hand) 3))

(defn four-of-a-kind? [hand]
  (x-of-a-kind (rank-frequencies hand) 4))

(defn flush? [hand]
  (x-of-a-kind (suit-frequencies hand) (count hand)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (= (x-of-a-kind-num (rank-frequencies hand) 2) 2)
      (four-of-a-kind? hand)))

(defn straight-sort [ranks]
  (let [replaced
        (if (and (= (apply min ranks) 2)
                 (= (apply max ranks) 14))
          (replace {14 1} ranks)
          ranks)]
    (sort replaced)))

(defn straight? [hand]
  (let [sorted (straight-sort (map rank hand))
        min (apply min sorted)
        max (apply max sorted)
        ref-range (range min (+ max 1))]
    (= sorted ref-range)))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn high-card? [hand]
  true)

(defn value2 [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        checked (filter (fn [[fun _]] (fun hand)) checkers)
        vals (map second checked)]
    (apply max vals)))

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

  (value2 straight-flush-hand) 

  (filter even? [1 2 3]))
