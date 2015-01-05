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

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  nil)

(defn four-of-a-kind? [hand]
  nil)

(defn flush? [hand]
  nil)

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
