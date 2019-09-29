(ns p-p-p-pokerface)

(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10, \9 9, \8 8, \7 7, \6 6, \5 5, \4 4, \3 3, \2 2})

(defn rank [card]
  (let [[r _] card]
    (replacements r)))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn- ranks [hand]
  (map rank hand))

(defn- freqs [hand]
  (frequencies (ranks hand)))

(defn- freqs-contains [hand c]
  (not (empty? (filter (fn [[a b]] (= c b)) (freqs hand)))))

(defn pair? [hand]
  (freqs-contains hand 2))

(defn three-of-a-kind? [hand]
  (freqs-contains hand 3))

(defn four-of-a-kind? [hand]
  (freqs-contains hand 4))

(defn flush? [hand]
  (not (empty? (filter (fn [[a b]] (= 5 b)) (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (freqs-contains hand 3) (freqs-contains hand 2)))

(defn two-pairs? [hand]
  (and (pair? hand) (= 3 (count (freqs hand)))))

(defn straight? [hand]
  (let [rs (ranks hand)
        sorted (sort rs)
        low (first sorted)
        scaled (map #(- % low) sorted)
        normal (range 0 5)
        ace (sort (conj (range 0 4) 12))]
    (or (= scaled normal) (= scaled ace))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (second (first (sort (fn [[a1 a2] [b1 b2]] (> a2 b2)) (filter (fn [[f v]] (f hand)) checkers))))
    ))
(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)