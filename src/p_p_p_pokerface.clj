(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card]
    (if (Character/isDigit rnk)
       (Integer/valueOf (str rnk))
       ({\T 10, \J 11, \Q 12, \K 13, \A 14} rnk))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (= 5 (first freqs))))


(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (and (contains? (set freqs) 3)
         (contains? (set freqs) 2))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (or (= 2 (get (frequencies freqs) 2))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        altranks (sort (replace {14 1} ranks))
        compseq (range (apply min ranks) (+ 1 (apply max ranks)))
        acompseq (range (apply min altranks) (+ 1 (apply max altranks)))]
    (or (= ranks compseq)
        (= altranks acompseq))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
