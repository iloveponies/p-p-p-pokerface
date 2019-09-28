(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if(Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(rank "2H") ;=> 2
(rank "4S") ;=> 4
(rank "TS") ;=> 10
(rank "JS") ;=> 11
(rank "QS") ;=> 12
(rank "KS") ;=> 13
(rank "AS") ;=> 14

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (if(< 1 (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn three-of-a-kind? [hand]
  (if(< 2 (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn four-of-a-kind? [hand]
  (if(< 3 (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn flush? [hand]
  (if(< 4 (apply max (vals (frequencies (map suit hand)))))
    true
    false))

(defn full-house? [hand]
  (if(and
          (= 3
            (apply max (vals (frequencies (map rank hand)))))
          (= 2
            (apply min (vals (frequencies (map rank hand))))))
    true
    false))

(defn two-pairs? [hand]
  (if(or
      (and
       (= 2
        (nth (sort (vals (frequencies (map rank hand)))) 1))
       (= 2
        (nth (sort (vals (frequencies (map rank hand)))) 2)))
      (< 3 (apply max (vals (frequencies (map rank hand))))))
    true
    false))

(defn straight? [hand]
  (or (= (sort (map rank hand)) (range (apply min (map rank hand)) (+ (apply min (map rank hand)) 5)))
      (= (sort (replace {14 1} (map rank hand))) (range (apply min (replace {14 1} (map rank hand))) (+ (apply min (replace {14 1} (map rank hand))) 5)))))

(defn straight-flush? [hand]
  (and (straight? hand)(flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [l [[high-card? 0]  [pair? 1]
         [two-pairs? 2]  [three-of-a-kind? 3]
         [straight? 4]   [flush? 5]
         [full-house? 6] [four-of-a-kind? 7]
         [straight-flush? 8]],
      f (fn [parameter]
        (let [[function val] parameter] (= true (function hand)))),
      w (fn [p]
          ( second p))]
  (apply max (map w (filter f l)))))

