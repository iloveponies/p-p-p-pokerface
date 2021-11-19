(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [[rank _]]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (get replacements rank)))

(defn suit [[_ suit]]
  (str suit))

(defn pair? [hand]
  (< 0 (count (filter #(= 2 %) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (< 0 (count (filter #(= 3 %) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (< 0 (count (filter #(= 4 %) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (= 1 (count (keys (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        inv-sorted-ranks (sort (replace {14 1} ranks))]
    (or (= (take 5 (iterate inc (first sorted-ranks))) sorted-ranks)
        (= (take 5 (iterate inc (first inv-sorted-ranks))) inv-sorted-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card?
  [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max
           (map second (filter (fn [[bool _]] bool)
                               (map (fn [[f score]] [(f hand) score]) checkers))))))
