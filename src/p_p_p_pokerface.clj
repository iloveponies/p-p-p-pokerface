(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (not (Character/isDigit fst))
      (get replacements fst)
      (Integer/valueOf (str fst)))))



(defn suit [card]
   (let [[_ snd] card]
    (str snd)))


(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))


(defn three-of-a-kind? [hand]
   (== 3 (apply max (vals (frequencies (map rank hand))))))



(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))



(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))




(defn full-house? [hand]
   (= '(2 3) (sort (vals (frequencies (sort (map rank hand)))))))



(defn two-pairs? [hand]
  (let [values (vals (frequencies (apply conj [] (map rank hand))))]
    (= '(1 2 2) (sort values))))



(defn straight? [hand]
  (let [sorted (apply conj [] (sort (map rank hand)))]
   (let [card-sequence? (fn [this-sorted]
                          (and (== 1 (apply max (vals (frequencies this-sorted))))
                               (== 4 (- (get this-sorted 4)(get this-sorted 0)))))]
    (let [new-sorted (apply conj [] (sort (replace {14 1} sorted)))]

       (if (== 1 (apply max (vals (frequencies sorted))))
         (if (== 2 (get sorted 0))
           (if (== 14 (get sorted 4))
             (card-sequence? new-sorted)
             (card-sequence? sorted))
           (card-sequence? sorted))
         false)))))


(defn straight-flush? [hand]
  (if (straight? hand)
    (flush? hand)
      false))

(defn high-card? [hand]
    0)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (let [callcheck (fn [item] (if ((get item 0) hand) (get item 1) 0))]
     (apply max (map callcheck checkers)))))

