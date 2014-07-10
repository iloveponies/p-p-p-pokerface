(ns p-p-p-pokerface
  (:require [test-data :refer :all]))

(defn suit [card]
  (let [[_ snd] card] (str snd)))

(defn rank [card]
  (let [[nm _] card]
  (get {\2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14} nm)))

(defn pair? [hand]
   (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))


(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) '(2 3)))


(defn two-pairs? [hand]
  (or
   (= (vals (frequencies (map rank hand))) '(2 2 1))
   (= (vals (frequencies (map rank hand))) '(4 1))))

(defn straight? [hand]
  (let [sorted-hand
        (fn[hand] (sort (keys (frequencies (map rank hand)))))

        range-helper
        (fn [hand] (= hand (range (apply min hand) (inc (apply max hand)))))

        low-ace
        (fn [hand] (range-helper (sort (replace {14 1} (sorted-hand hand)))))]
  (and
   (= (count (sorted-hand hand)) 5)
   (or
    (range-helper (sorted-hand hand))
    (low-ace hand )))))

(defn straight-flush? [hand]
  (and
   (= (vals (frequencies (map suit hand))) '(5))
   (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        ita (fn [coll] (if ((first coll) hand) (second coll)))]
   (apply max (filter (complement nil?) (map ita checkers)))))
