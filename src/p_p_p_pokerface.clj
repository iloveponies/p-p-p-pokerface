(ns p-p-p-pokerface)

(defn rank [card]
  (let [r (first card)
        ch->rank {\T 10, \J 11, \Q 12, \K 13 \A 14}]
    (if (Character/isDigit r)
      (read-string (str r))
      (ch->rank r))))

(defn- rank-freq-vals [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn- n-of-a-kind? [hand n]
  (let [freqs (rank-freq-vals hand)
        sets (filter #(= n %) freqs)]
    (not (empty? sets))))

(defn- successively? [coll]
  (let [n (- (count coll) 1)]
    (and (apply < coll)
         (= (- (last coll) (first coll)) n))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [freqs (sort compare (rank-freq-vals hand))]
    (= freqs [2 3])))

(defn two-pairs? [hand]
  (let [freqs (rank-freq-vals hand)
        sets (filter #(<= 2 %) freqs)]
    (= (apply + sets) 4)))

(defn straight? [hand]
  (let [ranks (sort compare (map rank hand))]
    (if (= (last ranks) 14)
      (or (successively? ranks)
          (successively? (concat [1] (take 4 ranks))))
      (successively? ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
