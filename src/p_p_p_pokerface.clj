(ns p-p-p-pokerface)

(defn rank [card]
  (let [[first _] card
        hc {\T 10
            \J 11
            \Q 12
            \K 13
            \A 14}]
    (if (Character/isDigit first) (Integer/valueOf (str first)) (get hc first))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (some #(= 2 %) (vals (frequencies (map rank hand)))))

(defn three-of-a-kind? [hand]
  (some #(= 3 %) (vals (frequencies (map rank hand)))))

(defn four-of-a-kind? [hand]
  (some #(= 4 %) (vals (frequencies (map rank hand)))))

(defn flush? [hand]
  (some #(= 5 %) (vals (frequencies (map suit hand)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (count (filter #(= 2 %) (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
