(ns p-p-p-pokerface)

(defn replacements [face-value]
  (let [values {\T 10, \J 11, \Q 12, \K 13, \A 14}
        result (get values face-value)]
    (Integer/valueOf (str (if (= nil result)
                            face-value
                            result)))))

(defn rank [card]
  (let [[fst _] card]
    (replacements fst)))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn rank-max-count [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (if (>= (rank-max-count hand) 2)
    true false))

(defn three-of-a-kind? [hand]
  (if (>= (rank-max-count hand) 3)
    true false))

(defn four-of-a-kind? [hand]
  (if (>= (rank-max-count hand) 4)
    true false))

(defn flush? [hand]
  (let [suit-freqs (apply
                    max (vals
                         (frequencies (map suit hand))))]
    (if (= suit-freqs (count hand))
      true false)))

(defn full-house? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (if (= (sort ranks) (range 2 4))
      true false)))

(defn two-pairs? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (if (or (= (sort ranks) (seq [1 2 2]))
            (= (sort ranks) (seq [1 4])))
      true false)))

(defn test-ranks [r]
  (let [f (first r)]
   (range f (+ f (count r)))))

(defn straight-seq? [hand]
  (let [ranks (sort (map rank hand))
        replaced-ace-ranks (sort (replace {14 1} ranks))]
    (if (or (= ranks (test-ranks ranks))
            (= replaced-ace-ranks (test-ranks replaced-ace-ranks)))
      true false)))

(defn straight? [hand]
  (let [two-suit-freqs? (>= (count (frequencies (map suit hand))) 2)]
    (if (and two-suit-freqs?
             (straight-seq? hand))
      true false)))

(defn straight-flush? [hand]
  (if (and (flush? hand)
           (straight-seq? hand))
    true false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map #(second %)
                    (filter (fn[x] ((first x) hand))
                            checkers)))
))

