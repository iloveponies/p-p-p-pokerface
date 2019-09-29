(ns p-p-p-pokerface)

(def poker-map {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst snd] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get poker-map fst))))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= [2 3]
     (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2]
     (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [high-ace (map rank hand)
        low-ace (replace {14 1} (map rank hand))
        high-ace-min (apply min high-ace)
        low-ace-min (apply min low-ace)]
    (or (= (range high-ace-min (+ high-ace-min 5))
           (sort high-ace))
        (= (range low-ace-min (+ low-ace-min 5))
           (sort low-ace)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))
