(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      ({\T 10, \J 11, \Q 12, \K 13, \A 14} r))))

(defn suit [card]
  (let [[_ s] card] (str s)))

; -- helper --
(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (let [freq (rank-frequencies hand)]
    (=
      (count (filter (fn [x] (= x 2)) freq))
      1)))

(defn three-of-a-kind? [hand]
  (let [freq (rank-frequencies hand)]
    (= (first (max freq)) 3)))

(defn four-of-a-kind? [hand]
  (let [freq (rank-frequencies hand)]
    (= (first (max freq)) 4)))

(defn suit-frequencies [hand]
  (vals (frequencies (map suit hand))))

(defn flush? [hand]
  (if (= (first (suit-frequencies hand)) 5) true false))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freq (rank-frequencies hand)]
    (=
      (count (filter (fn [x] (= x 2)) freq))
      2)))

(defn straight? [hand]
  (or
    (let [sorted (sort (map rank hand))]
      (= sorted (range (first sorted) (+ (first sorted) 5))))
    (let [sorted (sort (replace {14 1} (map rank hand)))]
      (= sorted (range (first sorted) (+ (first sorted) 5))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (= (first (suit-frequencies hand)) 5)))

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
