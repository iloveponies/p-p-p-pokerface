(ns p-p-p-pokerface)

(defn int-value [raw]
  (get {\T 10, \J 11, \Q 12, \K 13, \A 14} raw))

(defn rank [card]
  (let [[value suit] card]
    (if (Character/isDigit value)
      (Integer/valueOf (str value))
      (int-value value))))

(defn suit [card]
  (let [[value suit] card]
    (str suit)))

(rank "8S")

(defn has? [seq val]
  (not (empty? (filter (fn [x] (= x val)) seq))))

(defn pair? [hand]
    (has? (vals (frequencies (map rank hand))) 2))

(pair? ["3H" "9S" "4C" "5C" "7D"]);

(vals (frequencies (map rank ["2H" "2S" "4C" "5C" "7D"])))

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
