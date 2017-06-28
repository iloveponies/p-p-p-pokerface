(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
      (cond
        (= \T rank) 10
        (= \J rank) 11
        (= \Q rank) 12
        (= \K rank) 13
        (= \A rank) 14
        :else (Integer/valueOf(str rank)))))

(defn suit [card]
  (let [[_ suit ] card]
    (str suit)))

(defn pair? [hand]
  (let [card-counts (vals (frequencies (map rank hand)))
        maximum-card-count (apply max card-counts)]
    (>= 2 maximum-card-count)))

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
