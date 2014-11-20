(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [vals (vals (frequencies (map rank hand)))]
     (< 1 (apply max vals))))

(defn three-of-a-kind? [hand]
  (let [vals (vals (frequencies (map rank hand)))]
     (< 2 (apply max vals))))

(defn four-of-a-kind? [hand]
  (let [vals (vals (frequencies (map rank hand)))]
     (< 3 (apply max vals))))

(defn flush? [hand]
  (let [vals (vals (frequencies (map suit hand)))]
     (< 4 (apply max vals))))

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

