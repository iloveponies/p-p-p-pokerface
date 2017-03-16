(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        value-of-cards {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (value-of-cards fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (= 2 (some #{2} (vals freqs)))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (= 3 (some #{3} (vals freqs)))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (= 4 (some #{4} (vals freqs)))))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (frequencies suits)]
    (= 5 (some #{5} (vals freqs)))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (=
     '(2 3)
     (sort (vals freqs)))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (or
     (= '(1 2 2) (sort (vals freqs)))
     (= '(1 4) (sort (vals freqs))))))

(defn straight? [hand]
  (let [change-a {1 14, 14 1}
        sorted-ranks-1 (sort (map rank hand))
        sorted-ranks-14 (sort (replace change-a (map rank hand)))]
    (or
     (=
      (range (first sorted-ranks-1) (inc (last sorted-ranks-1)))
      sorted-ranks-1)
     (=
      (range (first sorted-ranks-14) (inc (last sorted-ranks-14)))
      sorted-ranks-14))))

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
    :else 0))
