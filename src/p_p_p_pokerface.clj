(ns p-p-p-pokerface)

(def char->rank
  {\T 10
   \J 11
   \Q 12
   \K 13
   \A 14})

(defn rank [card]
  (let [[^Character rank-char _] card]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (char->rank rank-char))))

(defn suit [card]
  (let [[_ suit-char] card]
    (str suit-char)))

(defn hand->ranks [hand]
  (map rank hand))

(defn n-of-a-kind? [n hand]
  (<= n (apply max (vals (frequencies (hand->ranks hand))))))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))


(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suit-set (set (map suit hand))]
    (= 1 (count suit-set))))

(defn sorted-rank-frequencies [hand]
  (sort (vals (frequencies (hand->ranks hand)))))

(defn full-house? [hand]
  (= [2 3] (sorted-rank-frequencies hand)))

(defn two-pairs? [hand]
  (let [sorted-rank-frequencies (sorted-rank-frequencies hand)]
    (or
      (= [1 2 2] sorted-rank-frequencies)
      (= [1 4] sorted-rank-frequencies))))

(defn straight? [hand]
  (let [ranks (hand->ranks hand)
        ranks-ace-as-1 (replace {14 1} ranks)
        ranks-have-straight? (fn [ranks]
                               (and
                                 (= 4 (- (apply max ranks) (apply min ranks)))
                                 (= 5 (count (set ranks)))))]
    (or
      (ranks-have-straight? ranks)
      (ranks-have-straight? ranks-ace-as-1))
    ))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn value [hand]
  (let [checkers #{[pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        points-for-checker (fn [[predicate points]]
                             (if (predicate hand) points 0))
        points-for-checkers (map points-for-checker checkers)]
    (apply max points-for-checkers)))
