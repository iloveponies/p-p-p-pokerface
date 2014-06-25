(ns p-p-p-pokerface)

(def replacements { \T 10, \J 11, \Q 12, \K 13, \A 14 })

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn- ranks [hand]
  (map rank hand))

(defn- frequencies-of-rank [hand]
  (vals (frequencies (ranks hand))))

(defn- max-rank [hand]
  (apply max (frequencies-of-rank hand)))

(defn pair? [hand]
  (= (max-rank hand) 2))

(defn three-of-a-kind? [hand]
  (= (max-rank hand) 3))

(defn four-of-a-kind? [hand]
  (= (max-rank hand) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= '(2 3) (sort (frequencies-of-rank hand))))

(defn two-pairs? [hand]
  (= '(1 2 2) (sort (frequencies-of-rank hand))))

(defn straight? [hand]
  (let [hand-ranks (ranks hand)]
    (letfn [(aux-straight? [hr]
      (let [min-rank (apply min hr)
            max-rank (apply max hr)]
        (= (range min-rank (inc max-rank)) (sort hr))))]
      (or (aux-straight? hand-ranks) (aux-straight? (replace {14 1} hand-ranks))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (filter identity (map (fn [[f? v]] (if (f? hand) v nil)) checkers)))))
