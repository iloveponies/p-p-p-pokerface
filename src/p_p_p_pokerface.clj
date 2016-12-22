(ns p-p-p-pokerface)

(defn rank [card]
  (let
    [[rank suit] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else
      ({\T 10,
        \J 11,
        \Q 12,
        \K 13,
        \A 14} rank))))

(defn suit [card]
  (let
    [[rank suit] card]
    (str suit)))

(defn rankfreqs [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn suitfreqs [hand]
  (apply max (vals (frequencies (map suit hand)))))

(defn pair? [hand]
  (< 1 (rankfreqs hand)))

(defn three-of-a-kind? [hand]
  (= 3 (rankfreqs hand)))

(defn four-of-a-kind? [hand]
  (= 4 (rankfreqs hand)))

(defn flush? [hand]
  (= 5 (suitfreqs hand)))

(defn full-house? [hand]
  (not (= 1 (apply min (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (let [[x y z] (sort (vals (frequencies (map rank hand))))]
    (cond
      (and
        (= 1 x)
        (= 2 y)
        (= 2 z))
        true
      :else
    false)))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        smallest (first sorted)
        alt (sort (replace {14 1} sorted))
        alt-smallest (first alt) ]
    (or
      (= sorted (range smallest (+ smallest 5)))
      (= alt (range alt-smallest (+ alt-smallest 5))))))


(defn straight-flush? [hand]
  (cond
    (and
      (flush? hand)
      (straight? hand))
    true
    :else
    false))

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
    :else
    0))
