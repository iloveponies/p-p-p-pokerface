(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
  (if (Character/isDigit fst) (Integer/valueOf (str fst)) (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (let [get-rank (fn [card] (rank card))
        found (fn [value] (<= 2 value))
        ranks (map get-rank hand)]
    (boolean (some found (vals (frequencies ranks))))))

(defn three-of-a-kind? [hand]
  (let [get-rank (fn [card] (rank card))
        found (fn [value] (<= 3 value))
        ranks (map get-rank hand)]
    (boolean (some found (vals (frequencies ranks))))))

(defn four-of-a-kind? [hand]
  (let [get-rank (fn [card] (rank card))
        found (fn [value] (<= 4 value))
        ranks (map get-rank hand)]
    (boolean (some found (vals (frequencies ranks))))))

(defn flush? [hand]
  (let [get-suit (fn [card] (suit card))
        found (fn [value] (= 5 value))
        suits (map get-suit hand)]
    (boolean (some found (vals (frequencies suits))))))

(defn full-house? [hand]
  (let [get-rank (fn [card] (rank card))
        found-2 (fn [value] (= 2 value))
        found-3 (fn [value] (= 3 value))
        ranks (map get-rank hand)]
    (boolean (and (some found-2 (vals (frequencies ranks))) (some found-3 (vals (frequencies ranks)))))))

(defn two-pairs? [hand]
  (let [get-rank (fn [card] (rank card))
        found-pair (fn [value] (<= 2 value))
        ranks (map get-rank hand)
        found-pairs (filter found-pair (vals (frequencies ranks)))]
    (boolean (= 2 (count found-pairs)))))

(defn straight? [hand]
  (let [get-rank (fn [card] (rank card))
        found-single (fn [value] (= 1 value))
        ranks (map get-rank hand)
        sorted-ranks (sort ranks)
        first-rank (nth sorted-ranks 0)
        second-last-rank (nth sorted-ranks 3)
        last-rank (nth sorted-ranks 4)
        found-single-ranks (filter found-single (vals (frequencies ranks)))]
    (boolean (and (= 5 (count found-single-ranks)) (or (= 4 (- last-rank first-rank)) (if (= last-rank 14) (= 4 (- second-last-rank 1))))))))

(defn straight-flush? [hand]
  (boolean (and (flush? hand) (straight? hand))))

(defn value [hand]
  (if (straight-flush? hand) 8
    (if (four-of-a-kind? hand) 7
      (if (full-house? hand) 6
      (if (flush? hand) 5
        (if (straight? hand) 4
          (if (three-of-a-kind? hand) 3
            (if (two-pairs? hand) 2
              (if (pair? hand) 1 0)))))))))
