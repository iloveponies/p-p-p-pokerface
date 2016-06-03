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
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (let [ranks (vec (sort (vals (frequencies (map rank hand)))))]
    (== 2 (count ranks) (get ranks 0))))

(defn two-pairs? [hand]
  (let [ranks (vec (sort (vals (frequencies (map rank hand)))))]
    (<= 4 (apply + (filter (fn [rank] (<= 2 rank)) ranks)))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
