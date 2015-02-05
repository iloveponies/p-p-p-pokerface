(ns p-p-p-pokerface)

(defn rank [card]
  (let [[value suit] card
    replacements {"T" 10, "J" 11, "Q" 12, "K" 13, "A" 14}]
    (if (Character/isDigit value)
    (Integer/valueOf (str value))
    (replacements (str value)))))

(defn suit [card]
  (let [[value suit] card]
    (str suit)))

(defn pair? [hand]
  (if (= 2 (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn three-of-a-kind? [hand]
  (if (= 3 (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn four-of-a-kind? [hand]
  (if (= 4 (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn flush? [hand]
  (if (= (count hand) (apply max (vals (frequencies (map suit hand)))))
    true
    false))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [pieni (sort (replace {14 1} (map rank hand)))
    suuri (sort (map rank hand))]
    (or
      (= pieni (range (first pieni) (+ 5 (first pieni))))
      (= suuri (range (first suuri) (+ 5 (first suuri)))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let[checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
