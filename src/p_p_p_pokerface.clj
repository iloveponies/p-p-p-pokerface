(ns p-p-p-pokerface)

(defn rank [card]
  (let [[first _] card]
    (if (Character/isDigit first)
      (Integer/valueOf (str first))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} first))))

(defn suit [card]
  (let [[_ second] card]
    (str second)))

(defn pair? [hand]
  (= 1 (count (filter
                (fn [x] (== x 2))
                (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (= 1 (count (filter
                (fn [x] (== x 3))
                (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (= 1 (count (filter
                (fn [x] (== x 4))
                (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (= 1 (count (filter
                (fn [x] (== x 5))
                (vals (frequencies (map suit hand)))))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (= 2 (count (filter
                (fn [x] (== x 2))
                (vals (frequencies (map rank hand)))))))

(defn straight-high-ace? [hand]
  (let [sorted-rank (sort (map rank hand))
        min-rank    (apply min sorted-rank)]
        (= sorted-rank (range min-rank (+ min-rank 5)))))

(defn straight-low-ace? [hand]
  (let [low-ace-hand (replace {"AH" "1H", "AS" "1S", "AD" "1D", "AC" "1C"} hand)
        sorted-rank  (sort (map rank low-ace-hand))
        min-rank     (apply min sorted-rank)]
        (= sorted-rank (range min-rank (+ min-rank 5)))))

(defn straight? [hand]
  (or (straight-high-ace? hand) (straight-low-ace? hand)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card?       0]
                   [pair?            1]
                   [two-pairs?       2]
                   [three-of-a-kind? 3]
                   [straight?        4]
                   [flush?           5]
                   [full-house?      6]
                   [four-of-a-kind?  7]
                   [straight-flush?  8]}]
       (apply max (map second (filter (fn [checker] ((first checker) hand)) checkers)))))
