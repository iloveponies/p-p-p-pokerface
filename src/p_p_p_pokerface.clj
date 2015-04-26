(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        face-card {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (cond
      (Character/isDigit fst) (Integer/valueOf (str fst))
      :else (get face-card fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (contains?
    (set (vals (frequencies (map rank hand))))
    2))

(defn three-of-a-kind? [hand]
  (contains?
    (set (vals (frequencies (map rank hand))))
    3))

(defn four-of-a-kind? [hand]
  (contains?
    (set (vals (frequencies (map rank hand))))
    4))

(defn flush? [hand]
  (=
    (count (set (keys (frequencies (map suit hand)))))
    1))

(defn full-house? [hand]
  (clojure.set/subset?
    #{2 3}
    (set (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (=
    (sort (vals (frequencies (map rank hand))))
    (seq [1 2 2])))

(defn straight? [hand]
  (let [ace-low (replace {14 1} (map rank hand))
        ace-high (map rank hand)
        checker (fn [x] (=
                  (sort x)
                  (range (apply min x) (+ (apply max x) 1))))]
    (or (checker ace-low)
        (checker ace-high))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max
           (map second
                (filter #((first %) hand) checkers)))))
