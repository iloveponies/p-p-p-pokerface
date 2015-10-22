(ns p-p-p-pokerface)

(def face-card-ranks
  {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get face-card-ranks r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (= 2 (some #{2} (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (some #{3} (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (some #{4} (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (some #{5} (vals (frequencies (map suit hand))))))

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
