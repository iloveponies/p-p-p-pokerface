(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (< 1 (apply max freqs))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (< 2 (apply max freqs))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (< 3 (apply max freqs))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= [2 3] (sort freqs))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (or
      (= [1 2 2] freqs)
      (= [1 4] freqs))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
