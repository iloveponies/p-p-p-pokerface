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
  (keys(map (fn [card] (rank card)) hand)))

(full-house? ["2H" "5D" "2D" "2C" "5S"])

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)

