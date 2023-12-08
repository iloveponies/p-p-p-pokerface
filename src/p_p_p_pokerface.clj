(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card]
  (cond
   (Character/isDigit rnk) (Integer/valueOf (str rnk))
   :else ({\T 10, \J 11, \Q 12, \K 13, \A 14} rnk))))


(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (if (=
       (apply max (vals ( frequencies (map rank hand))))
          2) true false))

(defn three-of-a-kind? [hand]
   (if (=
       (apply max (vals ( frequencies (map rank hand))))
          3) true false))

(defn four-of-a-kind? [hand]
   (if (=
       (apply max (vals ( frequencies (map rank hand))))
          4) true false))

(defn flush? [hand]
    (if (=
       (apply max (vals ( frequencies (map suit hand))))
          5) true false))

(defn full-house? [hand]
  (let [ srt (sort hand)
    [c1 c2 c3 c4 c5] srt]
    (cond
          (and (pair? [c1 c2])(three-of-a-kind? [c3 c4 c5])) true
          (and (pair? [c4 c5])(three-of-a-kind? [c1 c2 c3])) true
          :else false)))


(defn two-pairs? [hand]
  (let [ srt (sort hand)
       [c1 c2 c3 c4 c5] srt]
    (cond
       (and (pair? [c1 c2])(pair? [c3 c4])) true
       (and (pair? [c1 c2])(pair? [c4 c5])) true
       (and (pair? [c2 c3])(pair? [c4 c5])) true
       (four-of-a-kind? hand) true
     :else false)))


(defn straight? [hand]
  (let [crd1 (map rank hand)
    crd2 (replace {14 1} crd1)
    srt1 (sort crd1)
    srt2 (sort crd2)
    suora1 (range (apply min srt1)(inc (apply max srt1)))
    suora2 (range (apply min srt2)(inc (apply max srt2)))]
  (or (= srt1 suora1)(= srt2 suora2))))




(defn straight-flush? [hand]
  (and (flush? hand)(straight? hand)))


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
   :else 0 ))


