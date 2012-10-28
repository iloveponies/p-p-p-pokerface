(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        ranks { \T 10, \J 11, \Q 12, \K 13, \A 14 }
        ctoi (fn [x] (Integer/valueOf (str x)))]
    (if (Character/isDigit rank) (ctoi rank) (ranks rank))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [rankHand (map rank hand)
        comp     (fn [x] (== x 2))]
    (== 1 (count (filter comp (vals (frequencies rankHand)))))))

(defn three-of-a-kind? [hand]
  (let [rankHand (map rank hand)
        comp     (fn [x] (== x 3))]
    (== 1 (count (filter comp (vals (frequencies rankHand)))))))

(defn four-of-a-kind? [hand]
  (let [rankHand (map rank hand)
        comp     (fn [x] (== x 4))]
    (== 1 (count (filter comp (vals (frequencies rankHand)))))))

(defn flush? [hand]
  (let [suitHand (map suit hand)]
    (== 1 (count (vals (frequencies suitHand))))))

(defn full-house? [hand]
  (let [rankHand (map rank hand)]
    (and (pair? hand) (three-of-a-kind? hand))))

(defn two-pairs? [hand]
  (let [rankHand (map rank hand)
        comp     (fn [x] (== x 2))]
    (or (== 2 (count (filter comp (vals (frequencies rankHand)))))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
