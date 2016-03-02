(ns p-p-p-pokerface)

(def card-map {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (card-map r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [rank-frequencies (vals (frequencies (map rank hand)))]
    (and
     (= (apply max rank-frequencies) 2)
     (= (count (filter (fn [x] (= x 2)) rank-frequencies)) 1))))
                                 

(defn three-of-a-kind? [hand]
  (let [rank-frequencies (vals (frequencies (map rank hand)))]
    (and
     (= (apply max rank-frequencies) 3)
     (= (count (filter (fn [x] (= x 2)) rank-frequencies)) 0))))

(defn four-of-a-kind? [hand]
  (let [rank-frequencies (vals (frequencies (map rank hand)))]
    (= (apply max rank-frequencies) 4)))

(defn flush? [hand]
  (= (count (frequencies (map suit hand))) 1))

(defn full-house? [hand]
  (let [rank-frequencies (vals (frequencies (map rank hand)))]
    (= (sort rank-frequencies) (range 2 4))))

(defn two-pairs? [hand]
  (let [rank-frequencies (vals (frequencies (map rank hand)))
        sorted-freq (sort rank-frequencies)]
    (or (= sorted-freq [1 2 2]) (= sorted-freq [1 4]))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-ace (replace {14 1} ranks)
        sorted-ranks (sort ranks)
        sorted-ace (sort ranks-ace)
        first-seq (first sorted-ranks)
        first-ace (first sorted-ace)
        suit-count (count (frequencies (map suit hand)))]
    (or
     (and (= sorted-ranks (range first-seq (+ first-seq 5))) (> suit-count 1))
     (and (= sorted-ace (range first-ace (+ first-ace 5))) (> suit-count 1)))))
         

(defn straight-flush? [hand]
  (let [ranks (map rank hand)
        ranks-ace (replace {14 1} ranks)
        sorted-ranks (sort ranks)
        sorted-ace (sort ranks-ace)
        first-seq (first sorted-ranks)
        first-ace (first sorted-ace)
        suit-count (count (frequencies (map suit hand)))]
    (or
     (and (= sorted-ranks (range first-seq (+ first-seq 5))) (flush? hand))
     (and (= sorted-ace (range first-ace (+ first-ace 5))) (flush? hand)))))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
