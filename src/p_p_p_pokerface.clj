(ns p-p-p-pokerface)

(def replacements {\T 10
                   \J 11
                   \Q 12
                   \K 13
                   \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn max-count-of-same-rank [hand]
  (apply max
         (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (= 2 (max-count-of-same-rank hand)))

(defn three-of-a-kind? [hand]
  (= 3 (max-count-of-same-rank hand)))

(defn four-of-a-kind? [hand]
  (= 4 (max-count-of-same-rank hand)))

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
