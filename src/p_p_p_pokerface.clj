(ns p-p-p-pokerface)

(def replacements {\T 10
                   \J 11
                   \Q 12
                   \K 13
                   \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn count-same-rank-cards [hand n]
  (let [wanted-count? (fn [x] (= n x))]
    (count (filter wanted-count?
                   (vals (frequencies
                           (map rank hand)))))))

(defn pair? [hand]
  (= 1 (count-same-rank-cards hand 2)))

(defn three-of-a-kind? [hand]
  (= 1 (count-same-rank-cards hand 3)))

(defn four-of-a-kind? [hand]
  (= 1 (count-same-rank-cards hand 4)))

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (= 2 (count-same-rank-cards hand 2)))

(defn- aced-ranks [hand]
  (if (= 2 (apply min (map rank hand)))
    (replace {14 1} (map rank hand))
    (map rank hand)))

(defn straight? [hand]
  (let [ranks (aced-ranks hand)
        start (apply min ranks)
        end (apply max ranks)]
    (= (range start (inc end))
       (sort ranks))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check-hand (fn [checker] ((first checker) hand))]
        (let [hands-present (filter check-hand checkers)
              scores (map second hands-present)]
          (apply max scores))))
