(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
(defn rank [[v _]]
  (cond
    (Character/isDigit v) (Integer/valueOf (str v))
    (contains? replacements v) (get replacements v)
    :else 0))

(defn suit [[_ s]]
  (str s))

(defn pair? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (boolean (let [freqs (vals (frequencies (map rank hand)))]
    (and (some #(= % 2) freqs) 
         (some #(= % 3) freqs)))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
    (let [freqs (vals (frequencies (map rank hand)))]
      (== (count (filter #(== % 2) freqs)) 2))))

(defn successor-sequence? [a-seq]
  (every? #{-1} (map - a-seq (rest a-seq))))

(defn straight? [hand]
  (or
    (let [v (sort (map rank hand))]
      (successor-sequence? v))
    (let [v (sort (replace {2 15, 3 16, 4 17, 5 18} (map rank hand)))]
      (successor-sequence? v))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] 
                   [two-pairs? 2] [three-of-a-kind? 3] 
                   [straight? 4] [flush? 5] 
                   [full-house? 6] [four-of-a-kind? 7] 
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))
