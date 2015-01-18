(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get { \T 10, \J 11, \Q 12, \K 13, \A 14 } rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn- rank-freqs [hand]
  (vals (frequencies (map rank hand))))

(defn- n-of-a-kind [hand n]
  (= n (apply max (rank-freqs hand))))

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= '(2 3) (sort (rank-freqs hand))))

(defn two-pairs? [hand]
  (= '(1 2 2) (sort (rank-freqs hand))))

(defn- adjacent? [ranks]
  (let [sorted (sort ranks)
        lowest (first sorted)
        expected (range lowest (+ lowest 5))]
    (= sorted expected)))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (or (adjacent? (replace {14 1} ranks))
        (adjacent? ranks))))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
