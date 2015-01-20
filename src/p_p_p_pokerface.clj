(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
(def replacements-high-ace {\T 10, \J 11, \Q 12, \K 13, \A 14})
(def replacements-low-ace {\T 10, \J 11, \Q 12, \K 13, \A 1})

(defn rank [[rank]]
  (cond
   (Character/isDigit rank) (Integer/valueOf (str rank))
   :else (replacements rank)))

(defn suit [[_ suit]]
  (str suit))

(defn max-freq-rank [hand]
  (let [ranks (map rank hand)]
    (apply max (vals (frequencies ranks)))))

(defn pair? [hand]
  (>= (max-freq-rank hand) 2))

(defn three-of-a-kind? [hand]
  (>= (max-freq-rank hand) 3))

(defn four-of-a-kind? [hand]
  (>= (max-freq-rank hand) 4))

(defn suits [hand]
  (map suit hand))

(defn max-freq-suit [hand]
    (apply max (vals (frequencies (suits hand)))))

(defn flush? [hand]
  (= (max-freq-suit hand) 5))

(defn min-freq-rank [hand]
  (let [ranks (map rank hand)]
    (apply min (vals (frequencies ranks)))))

(defn ranks [hand]
  (map rank hand))

(defn ranks-no-replace [hand]
  (map first hand))

(defn full-house? [hand]
  (and (= (min-freq-rank hand) 2) (= (max-freq-rank hand) 3)))

(defn hand-freqs-sorted [hand]
  (sort (vals (frequencies (ranks hand)))))

(defn two-pairs? [hand]
  (let [two-pairs-freq (seq [1 2 2])]
    (or (= four-of-a-kind? hand) (= (hand-freqs-sorted hand) two-pairs-freq))))

(defn numberify [hand number-mapping]
  (let [char-to-int (fn [char] (Integer/valueOf (str char)))]
    (sort (map char-to-int (replace number-mapping (ranks-no-replace hand))))))

(defn sequence? [head ranks-as-numbers]
  (cond
   (empty? ranks-as-numbers) true
   (= (+ head 1) (first ranks-as-numbers)) (sequence?
                                             (first ranks-as-numbers)
                                             (rest ranks-as-numbers))
    :else false))

(defn straight? [hand]
  (let [ranks-as-numbers-high-ace (numberify hand replacements-high-ace)
        ranks-as-numbers-low-ace (numberify hand replacements-low-ace)]
    (or (sequence? (first ranks-as-numbers-high-ace) (rest ranks-as-numbers-high-ace))
        (sequence? (first ranks-as-numbers-low-ace) (rest ranks-as-numbers-low-ace)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        matches? (fn [[matcher value]]
                   (matcher hand))]
    (apply max (map second (filter matches? checkers)))))

