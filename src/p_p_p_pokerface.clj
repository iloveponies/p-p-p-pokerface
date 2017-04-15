(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst snd] card
       ranknums {\A 14, \K 13, \Q 12, \J 11, \T 10}]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst)) ;truthy, digit to integer
    (get ranknums fst) ;falsey, resolve integer value
    )))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn pair? [hand]
  (== 2 (apply max(vals (frequencies (map (fn [card] (rank card)) hand))))))


;(def high-seven ["2H" "3S" "4C" "5C" "7D"])
;(def pair-hand ["2H" "2S" "4C" "5C" "7D"])

;(pair? pair-hand)
;(pair? high-seven)

;(pair? ["2H" "2S" "4C" "5C" "7D"])
;(pair? ["2S" "4S" "4C" "9D" "KS"])

(defn three-of-a-kind? [hand]
  (== 3 (apply max(vals (frequencies (map (fn [card] (rank card)) hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max(vals (frequencies (map (fn [card] (rank card)) hand))))))

(defn flush? [hand]
  (== 5 (apply max(vals (frequencies (map (fn [card] (suit card)) hand))))))

(defn full-house? [hand]
  (let [value-counts (max(vals (frequencies (map (fn [card] (rank card)) hand))))
        [a b] value-counts]
    (and(count value-counts) (and (== a 3) (== b 2)))))


;(full-house? ["2H" "5D" "2D" "2C" "5S"])

(defn two-pairs? [hand]
  (let [value-counts (max(vals (frequencies (map (fn [card] (rank card)) hand))))
        [a b] value-counts]
    (and(count value-counts) (and (== a 2) (== b 2)))))


;(two-pairs? ["2H" "2S" "4C" "4D" "7D"])

(defn straight? [hand]
  (let [ranks (map (fn [card] (rank card)) hand)
        ranks-sorted (sort ranks)
        thingie (range (first ranks-sorted) (+ 5 (first ranks-sorted)))
        alter-thingie (drop-last thingie)
        alter-ranks-sorted (drop-last ranks-sorted)]

    (or (and (= alter-thingie alter-ranks-sorted) (= (last ranks-sorted) 14))
        (= thingie ranks-sorted))))


;the solution found above is both rickety and highly experimental

;(straight? ["2H" "3S" "6C" "5D" "4D"])
;(straight? ["2D" "3D" "4D" "5D" "AD"])
;(straight? ["2D" "3D" "4D" "5D" "KD"])

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  (not (or (flush? hand) (straight? hand) (pair? hand) (three-of-a-kind? hand))))


;(high-card? ["2H" "3S" "4C" "5C" "7D"])

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0))


;(value ["2H" "3S" "4C" "5C" "7D"])

