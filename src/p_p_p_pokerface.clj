(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (cond
      (Character/isDigit fst) (Integer/valueOf (str fst))
      :else (get { \T 10, \J 11, \Q, 12 \K, 13 \A, 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (= (get (frequencies ranks) 2) 1)))

(defn three-of-a-kind? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (= (get (frequencies ranks) 3) 1)))

(defn four-of-a-kind? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (= (get (frequencies ranks) 4) 1)))

(defn flush? [hand]
  (let [suits (map suit hand)]
   (= (count (set suits)) 1)))

(defn full-house? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
        (and (= (get (frequencies ranks) 2) 1)
         (= (get (frequencies ranks) 3) 1))))

(defn two-pairs? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (= (get (frequencies ranks) 2) 2)))

(defn straight? [hand]
  (let [sorted(sort (map rank hand))
        smallest-sorted(first sorted)
        low-sorted(sort (replace {14 1} (map rank hand)))
        smallest-low-sorted(first low-sorted)]
    (or (= sorted (range smallest-sorted (+ smallest-sorted 5)))
        (= low-sorted (range smallest-low-sorted (+ smallest-low-sorted 5))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
