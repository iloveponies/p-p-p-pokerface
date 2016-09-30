(ns p-p-p-pokerface)

(def replacements
  {\T 10,
   \J 11,
   \Q 12,
   \K 13,
   \A 14})

(def alt-replacements
  {\T 10,
   \J 11,
   \Q 12,
   \K 13,
   \A 1})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacements r))))

(defn alt-rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (alt-replacements r))))

(defn suit [card]
  (let [[_ d] card]
    (str d)))

(defn pair? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (== (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [freq-set (set (vals (frequencies (map rank hand))))]
    (and (contains? freq-set 2) (contains? freq-set 3))))

(defn two-pairs? [hand]
  (let [freq-seq (vals (frequencies (map rank hand)))]
    (= ((frequencies freq-seq) 2) 2)))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        sorted-alt-ranks (sort (map alt-rank hand))
        min-rank (apply min sorted-ranks)
        min-alt-rank (apply min sorted-alt-ranks)
        is-straight? (fn [m-r s-rs]
                       (= (range m-r (+ m-r 5)) s-rs))]
    (or (is-straight? min-rank sorted-ranks) (is-straight? min-alt-rank sorted-alt-ranks))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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
