(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn rank-frequencies [hand]
  (let [ranks (map rank hand)]
    (vec (vals (frequencies ranks)))))

(defn pair? [hand]
  (not (not (some #{2} (rank-frequencies hand)))))

(defn three-of-a-kind? [hand]
  (not (not (some #{3} (rank-frequencies hand)))))

(defn four-of-a-kind? [hand]
  (not (not (some #{4} (rank-frequencies hand)))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (three-of-a-kind? hand)
       (pair? hand)))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (rank-frequencies hand))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ace-high-hand (sort ranks)
        ace-low-hand (sort (replace {14 1} ranks))
        stricly-increasing-hand? #(apply = 1 (map - (rest %) %))]
    (or
      (stricly-increasing-hand? ace-high-hand)
      (stricly-increasing-hand? ace-low-hand))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (apply = (map suit hand))))

(defn value [hand]
  (let [high-card? (fn [hand] true)
        checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matching-checkers (filter #((first %) hand) checkers)]
    (apply max (map second matching-checkers))))
