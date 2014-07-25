(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (or (= [1 2 2] freqs)
        (= [1 4] freqs))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        lowranks (sort (replace {14 1} ranks))
        lowcard (apply min ranks)
        highcard (apply max ranks)]
    (or (= [1 2 3 4 5] lowranks)
        (= (range lowcard (+ 1 highcard)) ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check (fn [[func n]]
                (if (= true (func hand)) n 0))]
    (apply max (map check checkers))))
