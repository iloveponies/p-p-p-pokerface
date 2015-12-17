(ns p-p-p-pokerface)

(defn rank [card]
  (let [high-ranks {\T 10, \J 11, \Q 12 \K 13 \A 14}]
    (let [[rk _] card]
      (if (Character/isDigit rk)
        (Integer/valueOf (str rk))
        (get high-ranks rk)))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn freqs [hand freq-fn]
  (vals (frequencies (map freq-fn hand))))

(defn has-frequency? [freq freq-fn hand]
  (not (empty?
        (filter (fn [x] (== x freq))
                (freqs hand freq-fn)))))

(defn pair? [hand]
  (has-frequency? 2 rank hand))

(defn three-of-a-kind? [hand]
  (has-frequency? 3 rank hand))

(defn four-of-a-kind? [hand]
  (has-frequency? 4 rank hand))

(defn flush? [hand]
  (has-frequency? 5 suit hand))

(defn full-house? [hand]
  (= [2 3] (sort (freqs hand rank))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) (= [1 2 2] (sort (freqs hand rank)))))

(defn straight? [hand]
  (let [strght (fn [s] (= (range (apply min s) (+ (apply max s) 1)) s))]
    (or
     (strght (sort (map rank hand)))
     (strght (sort (replace {14 1} (map rank hand)))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max
           (map (fn [x] (get x 1))
                (filter (fn [x] (apply (get x 0) [hand]))
                        checkers)))))
