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

(defn n-of-a-kind [hand n]
  (not (nil? (some #(= n %) (vals (frequencies (map rank hand)))))))

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (not (nil? (some #(= 5 %) (vals (frequencies (map suit hand)))))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= 2 (count (filter #(= 2 %) (vals (frequencies (map rank hand))))))))

(defn straight? [hand]
  )

(defn straight-flush? [hand]
  (and 
    (straight? hand)
    (flush? hand)))

(defn value [hand]
  nil)
