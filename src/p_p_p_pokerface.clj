(ns p-p-p-pokerface)

(def replacement {\T  10, \J 11, \Q 12, \K 13, \A 14})

(defn suit [card]
  (str (nth card 1)))


(defn rank [card]
  (let [s (nth card 0)]
    (if (Character/isDigit s) (Integer/valueOf (str s))
      (get replacement s))))

(defn n-of-a-kind [n hand]
  (not (nil? (some #(>= % n )(vals (frequencies (map rank hand)))))))

(def pair? (partial n-of-a-kind 2 ))


(def three-of-a-kind? (partial n-of-a-kind 3))

(def four-of-a-kind? (partial n-of-a-kind 4))


(defn flush? [hand]
  (= 1 (count (distinct (map suit hand)))))

(defn full-house? [hand]
  (let [ val (vals (frequencies (map rank hand)))]
    (not (nil? (and (some #(= 3 %) val) (some #(= 2 %) val))))))



(defn two-pairs? [hand]
  (let [ val (vals (frequencies (map rank hand)))]
    (= 2 (count (filter #(= 2 %) val)))))



(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ace-hand ranks
        one-hand (sort (replace {14 1} ranks))]
    (or (= (range (first ace-hand) (+ (first ace-hand) 5)) ace-hand) (= (range 1 6) one-hand))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand)    8
   (four-of-a-kind? hand)    7
   (full-house? hand)        6
   (flush? hand)             5
   (straight? hand)          4
   (three-of-a-kind? hand)   3
   (two-pairs? hand)         2
   (pair? hand)              1
   :else 0))

