(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card tens {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get tens fst))))

(defn suit [card]
  (let [[_ snd] card] (str snd)))

(defn pair? [hand]
  (let [h (map rank hand) freq (vals (frequencies h))]
    (if (and
         (= (apply max freq) 2)
         (= (apply max (vals (frequencies freq))) 3))
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [h (map rank hand) freq (vals (frequencies h))]
    (if (and
         (= (apply max freq) 3)
         (not (= (apply min freq) 2)))
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [h (map rank hand) freq (vals (frequencies h))]
    (if (= (apply max freq) 4)
      true
      false)))

(defn flush? [hand]
  (let [h (map suit hand) freq (vals (frequencies h))]
    (if (= (apply max freq) 5)
      true
      false)))

(defn full-house? [hand]
  (let [h (map rank hand) freq (vals (frequencies h))]
    (if (and
         (= (apply max freq) 3)
         (= (apply min freq) 2))
         true
         false)))

(defn two-pairs? [hand]
  (let [h (map rank hand) freq (vals (frequencies h)) n (apply max freq)]
    (if (or
         (= n 4)
         (and (= n 2) (= (apply max (vals (frequencies freq))) 2)))
      true
      false
      )))

(defn straight? [hand]
  (let [h (map rank hand) str (replace {14 1} h) check-1 (range (apply min h) (+ (apply max h) 1)) check-2 (range (apply min str) (+ (apply max str) 1))]
    (if (or (= (sort h) check-1) (= (sort str) check-2))
      true
      false)))

(defn straight-flush? [hand]
  (if (and
       (straight? hand)
       (flush? hand))
    true
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
   :else 0))


