(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}
        digit? (fn [c] (Character/isDigit c))
        int-value (fn [c] (Integer/valueOf (str c)))
        [rnk _] card]
    (if (digit? rnk)
      (int-value rnk)
      (replacements rnk))))

(defn suit [card]
  (let [[_ suut] card]
    (str suut)))

(defn num-of-a-kind? [n hand]
  (let [ranks (map rank hand)]
    (== n (apply max (vals (frequencies ranks))))))

(defn pair? [hand]
  (num-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (num-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (num-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [hand-size (count hand)
        suits (map suit hand)]
    (== hand-size (apply max (vals (frequencies suits))))))

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
