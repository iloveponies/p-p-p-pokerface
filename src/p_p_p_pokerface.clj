(ns p-p-p-pokerface)

(defn rank [card]
  (let [[frs _] card] (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
   (cond
    (Character/isDigit frs) (Integer/valueOf (str frs))
    :else (replacements frs))))

(defn suit [card]
  (let [[_ snd] card] (str snd)))

(defn pair? [hand]
  (let [rankT (vals (frequencies (map rank hand)))] (>= (apply max rankT) 2)))

(defn three-of-a-kind? [hand]
  (let [rankT (vals (frequencies (map rank hand)))] (>= (apply max rankT) 3)))

(defn four-of-a-kind? [hand]
  (let [rankT (vals (frequencies (map rank hand)))] (>= (apply max rankT) 4)))

(defn flush? [hand]
  (let [rankT (vals (frequencies (map suit hand)))] (== (apply max rankT) 5)))

(defn full-house? [hand]
  (let [rankT (vals (frequencies (map rank hand)))]
   (== (apply + (filter (fn [x] (> x 1)) rankT)) 5)))

(defn two-pairs? [hand]
  (let [rankT (vals (frequencies (map rank hand)))]
    (cond
     (four-of-a-kind? hand) true
     :else (== 2 (count (filter (fn [x] (== x 2)) rankT))))))

(defn straight? [hand]
  (let [sortedHand (sort (map rank hand))]
   (let [smallest (first sortedHand)]
     (let [highest (last sortedHand)]
        (cond
         (= sortedHand (range smallest (+ smallest 5))) true
         (== highest 14) (= (range 1 6) (conj (filter (fn [x] (< x 14)) sortedHand) 1 ))
         :else false)))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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

