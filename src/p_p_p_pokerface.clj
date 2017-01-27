(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))


(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= (count hand)
     (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (range 2 4)
     (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (<= 2
     (second (sort > (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let
    [sorted-hand (sort (map rank hand))
     sorted-ace-first-hand (sort (replace {14, 1} sorted-hand))
     is-increasing? (fn [h] (= (range (first h) (+ (first h) (count h)))
                                 h))]
  (or
    (is-increasing? sorted-hand)
    (is-increasing? sorted-ace-first-hand))))

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

