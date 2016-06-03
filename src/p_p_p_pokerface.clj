(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (Integer/valueOf (str (replacements fst))))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (< 1 (val (apply max-key val (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2 (val (apply max-key val (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (val (apply max-key val (frequencies (map rank hand))))))

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
