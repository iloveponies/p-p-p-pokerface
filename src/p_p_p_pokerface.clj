(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] [(first card) (str(second card))]]
    (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
      (if(Character/isDigit r)
        (Integer/valueOf (str r)) ;1-9 converted to integer
        (replacements r)))) ;character to number conversion

(defn suit [card]
  (let [[_ s] [(str(first card)) (str(second card))]]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        pairs (apply max(vals(frequencies ranks)))]
          (< 1 pairs)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        pairs (apply max(vals(frequencies ranks)))]
          (< 2 pairs)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        pairs (apply max(vals(frequencies ranks)))]
          (< 3 pairs)))

(defn flush? [hand]
  (let [suits (map suit hand)
        pairs (apply max(vals(frequencies suits)))]
          (= 5 pairs)))
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
