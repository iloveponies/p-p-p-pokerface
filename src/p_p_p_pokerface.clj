(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        vals {\T 10,\J 11,\Q 12,\K 13,\A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get vals fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        most (apply max (vals (frequencies ranks)))]
    (= most 2)))

(defn three-of-a-kind? [hand]
    (let [ranks (map rank hand)
        most (apply max (vals (frequencies ranks)))]
    (= most 3)))

(defn four-of-a-kind? [hand]
    (let [ranks (map rank hand)
        most (apply max (vals (frequencies ranks)))]
    (= most 4)))

(defn flush? [hand]
    (let [suits (map suit hand)
        most (apply max (vals (frequencies suits)))]
    (= most 5)))

(defn full-house? [hand]
    (let [ranks (map rank hand)
          rank-list (sort (vals (frequencies ranks)))]
    (= rank-list (range 2 4))))

(defn two-pairs? [hand]
    (let [ranks (map rank hand)
          rank-list (sort (vals (frequencies ranks)))]
    (= rank-list (seq [1 2 2]))))

(defn straight? [hand]
  (let [ranks (map rank hand)
       freqs (frequencies ranks)
       rank-list (sort (keys freqs))
       rank-freq (sort (vals freqs))
       spread (- (apply max (vec rank-list)) (apply min (vec rank-list)))]
  (or (= rank-list (seq [2 3 4 5 14]))
      (and (= spread 4) (= rank-freq (seq [1 1 1 1 1]))))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn value [hand]
 (cond
   (pair? hand) 1
   (two-pairs? hand) 2
   (three-of-a-kind? hand) 3
   (straight? hand) 4
   (flush? hand) 5
   (full-house? hand) 6
   (four-of-a-kind? hand) 7
   (straight-flush? hand) 8
   :else 0))


