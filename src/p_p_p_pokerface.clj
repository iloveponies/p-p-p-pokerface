(ns p-p-p-pokerface)

(defn rank [card]
  (let [[x _] card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (if (Character/isDigit x)(Integer/valueOf (str x))(replacements x)))
  )

(defn suit [card]
  (let [[_ x] card]
    (str x))
  )

(defn pair? [hand]
  (if (== 2 (apply max (vals (frequencies (map rank hand))))) true false)
 )

(defn three-of-a-kind? [hand]
  (if (== 3 (apply max (vals (frequencies (map rank hand))))) true false )
 )

(defn four-of-a-kind? [hand]
  (if (== 4 (apply max (vals (frequencies (map rank hand))))) true false )
 )

(defn flush? [hand]
  (if (== 5 (apply max (vals (frequencies (map suit hand))))) true false)
 )

(defn full-house? [hand]
  (let [x (vals (frequencies (map rank hand)))]
    (and (== 2 (apply min x))
         (== 3 (apply max x))))
  )

(defn two-pairs? [hand]
  (let [x (vals (frequencies (map rank hand)))
        y (vals (frequencies (vals (frequencies (map rank hand)))))]
    (or (== 4(apply max x))
        (and
          (== 2 (apply max x))
          (== 2 (apply max y)))))
  )

(defn straight? [hand]
  (let [x (sort (map rank hand))
        z (sort (replace {14 1} (map rank hand)))
        y (range (apply min (map rank hand)) (+ 1 (apply max (map rank hand))))
        f (range (apply min (replace {14 1} (map rank hand))) (+ 1 (apply max (replace {14 1} (map rank hand)))))]
  (or (= x y)
      (= z f)))
  )

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand))
  )

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        x (filter #((first %) hand) checkers)
        y (map #(second %) x)]
        (apply max y))
  )
