(ns p-p-p-pokerface)

(defn rank [card]
  (let [r (first card)]
    (cond
     (Character/isDigit r) (Integer/valueOf (str r))
     :else (get {\T 10, \J 11, \Q 12, \K 13, \A 14} r))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (let [fs (frequencies (map rank hand))]
    (boolean (some #(>= % 2) (vals fs)))))

(defn three-of-a-kind? [hand]
  (let [fs (frequencies (map rank hand))]
    (boolean (some #(>= % 3) (vals fs)))))

(defn four-of-a-kind? [hand]
  (let [fs (frequencies (map rank hand))]
    (boolean (some #(>= % 4) (vals fs)))))

(defn flush? [hand]
  (let [[x & xs] (map suit hand)]
    (every? #(= % x) xs)))

(defn full-house? [hand]
  (let [fs (frequencies (map rank hand))]
    (= (sort (vals fs)) '(2 3))))

(defn two-pairs? [hand]
  (or (let [fs (frequencies (map rank hand))]
        (= (count (filter #(= % 2) (vals fs))) 2))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [[x & xs :as rs] (sort (map rank hand))]
    (or (= rs (range x (+ x 5)))
        (= rs '(2 3 4 5 14)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand)  8
   (four-of-a-kind? hand)  7
   (full-house? hand)      6
   (flush? hand)           5
   (straight? hand)        4
   (three-of-a-kind? hand) 3
   (two-pairs? hand)       2
   (pair? hand)            1
   :else                   0))
