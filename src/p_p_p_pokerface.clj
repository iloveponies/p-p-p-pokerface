(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        repl {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (cond
      (Character/isDigit r) (Integer/valueOf (str r))
      :else (Integer/valueOf (str (repl r))) )))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(comment
;;For testing in REPL

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
  (def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"]))


(defn rank-seq [hand]
  (map rank hand))

(defn rank-frequencies [hand]
  (vals (frequencies (rank-seq hand))))

(defn n-of-a-kind? [n hand]
  (true? (some #(= n %) (rank-frequencies hand))))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suit-seq (keys (frequencies (map suit hand)))]
    (if (= (count suit-seq) 1) true false)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (cond
    (four-of-a-kind? hand) true
    (= (get (frequencies (rank-frequencies hand)) 2) 2) true
    :else false))

(defn straight? [hand]
  (let [r-s (sort (rank-seq hand))
        r-l (sort (replace {14 1} (rank-seq hand)))]
    (cond
      (or
       (= (range (first r-s) (+ 1 (last r-s))) r-s)
       (= (range (first r-l) (+ 1 (last r-l))) r-l)) true
      :else false)))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
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
