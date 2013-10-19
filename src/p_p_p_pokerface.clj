(ns p-p-p-pokerface)

(def replacements
  {\T 10
   \J 11
   \Q 12
   \K 13
   \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (and
      (contains? (set ranks) 2)
      (not (== (count (set ranks)) (count ranks))))))

(defn three-of-a-kind? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (and
      (contains? (set ranks) 3)
      (not (== (count (set ranks)) (count ranks))))))

(defn four-of-a-kind? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (contains? (set ranks) 4)))

(defn flush? [hand]
  (let [ranks (map rank hand)]
    (or (apply < ranks)
        (apply > ranks))))

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
