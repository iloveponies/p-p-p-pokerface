(ns p-p-p-pokerface)

(def rank-map {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[r _] card]
    (get rank-map r)))

(defn max-of-a-kind [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (>= (max-of-a-kind hand) 2))

(defn three-of-a-kind? [hand]
  (>= (max-of-a-kind hand) 3))

(defn four-of-a-kind? [hand]
  (>= (max-of-a-kind hand) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        v (set (vals (frequencies ranks)))]
    (and (contains? v 2)
         (contains? v 3))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        fval (vals (frequencies ranks))
        ffreq (frequencies fval)]
    (= (get ffreq 2) 2)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted (sort ranks)
        sorted2 (sort (replace {14 1} sorted))
        min-elem (apply min sorted)
        min-elem2 (apply min sorted2)
        expected (range min-elem (+ min-elem 5))
        expected2 (range min-elem2 (+ min-elem2 5))]
    (or (= sorted expected)
        (= sorted2 expected2))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        f (fn [[f score]] (if (f hand) score 0))]
    (apply max (map f checkers))))
