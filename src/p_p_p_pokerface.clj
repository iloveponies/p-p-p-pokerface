(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        bigCards {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get bigCards r))))


(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn has-freq? [i t m]
  (let [f (fn [e] (== e i))]
    (not
      (empty?
        (filter
          f
          (vals (frequencies (map t m))))))))

(defn pair? [hand]
  (has-freq? 2 rank hand))

(defn three-of-a-kind? [hand]
  (has-freq? 3 rank hand))

(defn four-of-a-kind? [hand]
  (has-freq? 4 rank hand))

(defn flush? [hand]
  (= 5 (count (frequencies (map rank hand)))))

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
