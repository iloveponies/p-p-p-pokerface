(ns p-p-p-pokerface)

(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[fst _] card]
  (cond
   (Character/isDigit fst) (Integer/valueOf (str fst))
   :else (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn suit-frequencies [hand]
  (vals (frequencies (map suit hand))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (let [freqs (rank-frequencies hand)]
    (<= 2 (apply max freqs))))

(defn three-of-a-kind? [hand]
  (let [freqs (rank-frequencies hand)]
    (<= 3 (apply max freqs))))

(defn four-of-a-kind? [hand]
  (let [freqs (rank-frequencies hand)]
    (<= 4 (apply max freqs))))

(defn flush? [hand]
  (let [freqs (suit-frequencies hand)]
    (= 5 (apply max freqs))))

(defn full-house? [hand]
  (let [freqs (sort (rank-frequencies hand))]
    (= freqs (seq [2 3]))))

(defn two-pairs? [hand]
  (let [freqs (sort (rank-frequencies hand))]
    (or
     (= freqs (seq [1 2 2]))
     (= freqs (seq [1 4])))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (if (and (= 14 (last ranks)) (= 2 (first ranks)))
      (every? #{1} (map - (rest (drop-last ranks)) (drop-last ranks)))
      (every? #{1} (map - (rest ranks) ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map (fn [x] (if (= true ((first x) hand)) (last x) 0)) checkers))))
