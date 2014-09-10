(ns p-p-p-pokerface)

(defn rank [card]
  (let [[v _] card]
    (if (Character/isDigit v)
      (Integer/valueOf (str v))
      ({\T 10 \J 11 \Q 12 \K 13 \A 14} v))))

(defn suit [card]
  (let [[_ s] card] (str s)))

(defn ranks [cards]
  (map rank cards))

(defn frequencies-of [hand freq]
  (filter #(= freq %) (vals (frequencies (ranks hand)))))

(defn has-frequency-of? [hand freq]
  (not (empty? (frequencies-of hand freq))))

(defn pair? [hand]
  (has-frequency-of? hand 2))

(defn three-of-a-kind? [hand]
  (has-frequency-of? hand 3))

(defn four-of-a-kind? [hand]
  (has-frequency-of? hand 4))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= [2 2] (frequencies-of hand 2)))

(defn straight? [hand]
  (let [[r-1 r-2 r-3 r-4 r-5 :as original-sorted] (sort (ranks hand))
        has-14-and-2? (and (= r-5 14) (= r-1 2))
        ranks (if has-14-and-2? [1 r-1 r-2 r-3 r-4] original-sorted)
        ranks-set (set ranks)
        has-5-unique-elements? (= 5 (count ranks-set))]
    (and (= (set (range (first ranks) (inc (last ranks)))) ranks-set) has-5-unique-elements?)))

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
