(ns p-p-p-pokerface)

(def face-rank-map {\T 10
                    \J 11
                    \Q 12
                    \K 13
                    \A 14})

(defn rank [card]
  (let [value (first card)]
    (if (Character/isDigit value)
      (Integer/valueOf (str value))
      (get face-rank-map value))))

(defn suit [card]
  (str (second card)))

(defn has-n-of-rank? [n hand]
  (let [freqs (vals (frequencies (map rank hand)))]
  (boolean (some #{n} freqs))))

(defn has-n-of-suit? [n hand]
  (let [freqs (vals (frequencies (map suit hand)))]
  (boolean (some #{n} freqs))))

(defn pair? [hand]
  (has-n-of-rank? 2 hand))

(defn three-of-a-kind? [hand]
  (has-n-of-rank? 3 hand))

(defn four-of-a-kind? [hand]
  (has-n-of-rank? 4 hand))

(defn flush? [hand]
  (has-n-of-suit? 5 hand))

(defn full-house? [hand]
    (and (three-of-a-kind? hand)
       (pair? hand)))

(defn two-pairs? [hand]
  (let [pairs (filter #(>= % 2) (vals (frequencies (map rank hand))))]
    (= 2 (count pairs))))

(defn possible-straight-hands [ranks]
  (let [ace (== (get face-rank-map \A)
                (apply max ranks))]
    (map sort (if ace
                [ranks (replace {14 1} ranks)]
                [ranks]))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        possible-hands (possible-straight-hands ranks)]
    (boolean (some #{true} (map (fn [h] (let [min-rank (apply min h)]
                   (= (range min-rank (+ min-rank 5)) h)))
           possible-hands)
    ))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [h] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map #(if ((first %) hand) (second %) 0) checkers))))

