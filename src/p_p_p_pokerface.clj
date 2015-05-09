(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn suit-count [hand]
  (count (keys (frequencies (map suit hand)))))

(defn same-kind [hand count]
  (boolean 
   (some #(== % count)
         (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (same-kind hand 2))

(defn three-of-a-kind? [hand]
  (same-kind hand 3))

(defn four-of-a-kind? [hand]
  (same-kind hand 4))

(defn flush? [hand]
  (if (== 5 
          (apply max 
           (vals (frequencies (map suit hand)))))
    true
    false))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= [1 2 2] 
     (sort (vals (frequencies (map rank hand))))))

(defn ascending? [sorted-col]
  (every? identity 
          (map 
           #(== 1 (- (nth % 1) (nth % 0)))
           (partition 2 1 sorted-col))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        with-1 (sort (replace {14 1} ranks))
        with-14 (sort (replace {1 14} ranks))]
    (or (ascending? with-1)
        (ascending? with-14))))

(defn straight-flush? [hand]
  (and (straight? hand)
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
           (map 
            #(second %) 
            (filter #((first %) hand) checkers)))))
