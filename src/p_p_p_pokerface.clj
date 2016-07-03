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

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
