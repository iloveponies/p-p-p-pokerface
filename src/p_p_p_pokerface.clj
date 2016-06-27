(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
        (if (Character/isDigit fst)
            (Integer/valueOf (str fst))
            ({\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
        (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
        (and 
          (= (count freqs) 4)
          (= (apply max freqs) 2))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
        (and
          (= (count freqs) 3)
          (= (apply max freqs) 3))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
        (= (apply max freqs) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
        (= (count (frequencies suits)) 1)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
        (= (sort freqs) (seq [2, 3]))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
        (= (sort freqs) (seq [1, 2, 2]))))

(defn straight? [hand]
  (let [ranks1 (map rank hand)
        ranks2 (replace {14 1} ranks1)
        f (fn [ranks]
            (let [freqs (vals (frequencies ranks))]
                  (and
                    (= (count freqs) 5)
                    (= (- (apply max ranks) (apply min ranks)) 4))))]
        (or 
          (f ranks1)
          (f ranks2))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

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
    :else 0
    ))
