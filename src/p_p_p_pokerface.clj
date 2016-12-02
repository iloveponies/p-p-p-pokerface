

















(ns p-p-p-pokerface)

(defn rank [card]
  (let [[f s] card]
    (if (Character/isDigit f)
      (Integer/valueOf (str f))
      (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
        (replacements f)))))

(defn suit [card]
  (let [[fst snd] card] (str snd)))


(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))


(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))


(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))


(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))


(defn full-house? [hand]
  (and (three-of-a-kind? hand) (= 2 (count (vals (frequencies (map rank hand)))))))


(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (and (not (three-of-a-kind? hand) )
           (= 3 (count (vals (frequencies (map rank hand))))))))


(defn straight? [hand]
  (let [sorted (sort (map rank hand))]
    (if (= sorted (range (apply min sorted) (+ 1 (apply max sorted))))
      (boolean true)
      (= [1 2 3 4 5] (sort (replace {14 1} sorted))))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (let [check (fn [x] ((get x 0) hand))]
      (apply max (map second (filter check checkers))))))
