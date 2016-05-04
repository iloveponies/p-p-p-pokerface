(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (let [letters {\A 14,
                 \T 10,
                 \J 11,
                 \Q 12,
                 \K 13}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get letters rank))))

(defn suit [[_ suit]] 
  (str suit))

(defn pair? [hand]
  (let [freq (frequencies (map rank hand))]
    (= 1 (count (filter (fn [[k v]] (and (= 0 (mod k 2)) (= 2 v))) freq)))))

(defn x-of-a-kind? [x hand]
  (let [freq (frequencies (map rank hand))]
    (= 1 (count (filter (fn [[_ v]] (= x v)) freq)))))

(defn three-of-a-kind? [hand]
  (x-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (x-of-a-kind? 4 hand))

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (and (= 2 (count values)) (or (= 3 (first values)) (= 3 (second values))))))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
