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

; private
(defn rank-count [hand]
  (frequencies (map rank hand)))

; private
(defn rank-counts [hand]
  (vals (rank-count hand)))

; private
(defn max-rank-counts [hand]
  (apply max (rank-counts hand)))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (= 2 (max-rank-counts hand)))

(defn three-of-a-kind? [hand]
  (= 3 (max-rank-counts hand)))

(defn four-of-a-kind? [hand]
  (= 4 (max-rank-counts hand)))

(defn flush? [hand]
  (apply = (map suit hand)))

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
