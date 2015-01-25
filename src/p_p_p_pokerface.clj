(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r) (Integer/valueOf (str r)) (replacements r))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (> (count (filter (fn [x] (== x 2)) (vals(frequencies (map rank hand))))) 0))

(defn three-of-a-kind? [hand]
  (> (count (filter (fn [x] (== x 3)) (vals(frequencies (map rank hand))))) 0))

(defn four-of-a-kind? [hand]
  (> (count (filter (fn [x] (== x 4)) (vals(frequencies (map rank hand))))) 0))

(defn flush? [hand]
  (> (count (filter (fn [x] (== x 5)) (vals(frequencies (map suit hand))))) 0))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (> (count (filter (fn [x] (== x 2)) (vals(frequencies (map rank hand))))) 1))

(defn straight? [hand]
  (let [nr (fn [r] (= (range (apply min r) (+ (apply min r) 5) ) (sort r)))]
    (or (nr (map rank hand)) (nr (map (fn [x] (mod (- (rank x) 1) 13)) hand)) )
    )
  )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
   :else                   0
   ))
