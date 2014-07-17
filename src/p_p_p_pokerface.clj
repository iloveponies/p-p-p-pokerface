(ns p-p-p-pokerface)

(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacements rank))))

(defn ranks [hand]
  (map rank hand))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn suits [hand]
  (map suit hand))

(defn rank-values [hand]
  (vals (frequencies (ranks hand))))

(defn suit-values [hand]
  (vals (frequencies (suits hand))))

(defn contains-x-same-ranks [x hand]
  (contains? (set (rank-values hand)) x))

(defn pair? [hand]
  (contains-x-same-ranks 2 hand))

(defn three-of-a-kind? [hand]
  (contains-x-same-ranks 3 hand))

(defn four-of-a-kind? [hand]
  (contains-x-same-ranks 4 hand))

(defn flush? [hand]
  (contains? (set (suit-values hand)) 5))

(defn full-house? [hand]
  (= #{2 3} (set (rank-values hand))))

(defn two-pairs? [hand]
  (let [freqs (filter (fn [x] (not (= 1 x))) (rank-values hand))]
    (not (= (count freqs) (count (set freqs))))))

(defn straight? [hand]
  (let [r (sort (ranks hand))
        min-r (reduce min r)
        r-reduced (map (fn [x] (- x min-r)) r)
        replaced (sort (replace {14 1} r))
        min-replaced (reduce min replaced)
        replaced-reduced (map (fn [x] (- x min-replaced)) replaced)
        straight (range 0 5)]
    (if (or (= r-reduced straight) (= replaced-reduced straight)) true false)))
    

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
