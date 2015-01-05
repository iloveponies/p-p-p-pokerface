(ns p-p-p-pokerface)

(defn rank [card]
  (let [ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get ranks r))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (true? (some #(< 1 %) (vals freqs)))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (true? (some #(< 2 %) (vals freqs)))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (true? (some #(< 3 %) (vals freqs)))))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (frequencies suits)]
    (= 5 (first (vals freqs)))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (= [2 3] (sort (vals freqs)))))

(defn two-pairs? [hand]
    (let [ranks (map rank hand)
          freqs (frequencies ranks)
          freq-vals (vals freqs)
          pairs (filter #(< 1 %) freq-vals)]
      (< 1 (count pairs))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        low-ace (replace {14 1} ranks)
        sorted-low-ace (sort (replace {14 1} ranks))
        first-rank (first sorted-ranks)
        first-low-ace (first sorted-low-ace)]
    (true? (or
            (= sorted-ranks (range first-rank (+ first-rank 5)))
            (= sorted-low-ace (range first-low-ace (+ first-low-ace 5)))))))

(defn straight-flush? [hand]
  (true? (and (straight? hand)
              (flush? hand))))

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
