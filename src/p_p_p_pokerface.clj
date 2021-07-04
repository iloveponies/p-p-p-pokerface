(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (let [rank-freq (vals (frequencies (map rank hand)))]
    (= [1 1 1 2] (sort rank-freq))))

(defn two-pairs? [hand]
  (let [rank-freq (vals (frequencies (map rank hand)))]
    (= [1 2 2] (sort rank-freq))))

(defn three-of-a-kind? [hand]
  (let [rank-freq (vals (frequencies (map rank hand)))]
    (= [1 1 3] (sort rank-freq))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        rank-min (apply min ranks)]
    (and
     (or
      (= (range rank-min (+ rank-min 5)) (sort ranks))
      (= (range 1 6) (sort (replace {14 1} ranks))))
     (> (count (set (map suit hand))) 1))))

(defn flush? [hand]
  (let [ranks (map rank hand)
        rank-min (apply min ranks)]
    (and
     (== (count (set (map suit hand))) 1)
     (not (= (range rank-min (+ rank-min 5)) (sort ranks)))
     (not (= (range 1 6) (sort (replace {14 1} ranks)))))))

(defn full-house? [hand]
  (let [rank-freq (vals (frequencies (map rank hand)))]
    (= [2 3] (sort rank-freq))))

(defn four-of-a-kind? [hand]
  (let [rank-freq (vals (frequencies (map rank hand)))]
    (= [1 4] (sort rank-freq))))

(defn straight-flush? [hand]
  (let [ranks (map rank hand)
        rank-min (apply min ranks)]
    (and
     (or
      (= (range rank-min (+ rank-min 5)) (sort ranks))
      (= (range 1 6) (sort (replace {14 1} ranks))))
     (== (count (set (map suit hand))) 1))))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2]
                   [three-of-a-kind? 3] [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}]
    (apply max (map second (filter (fn [[check _]] (check hand)) checkers)))))
