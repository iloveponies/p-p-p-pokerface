(ns p-p-p-pokerface)

(defn rank [card]
  (let [facecards {\A 14, \J 11, \Q 12, \K 13, \T 10}]
   (cond
     (Character/isDigit (get card 0)) (Integer/valueOf (str (get card 0)))
     :else (facecards (get card 0)))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (apply = 5 (vals (frequencies (map suit hand)))))

(defn full-house? [hand]
  (= (range 2 4) (sort(vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= (seq [1 2 2]) (sort(vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [alt-ace-hand (fn [] (replace {14, 1} (map rank hand)))]
    (cond
     (and (= 4 (- (apply max (map rank hand))
                  (apply min (map rank hand))))
          (apply < (sort (map rank hand)))) true
     (and (= 4 (- (apply max (alt-ace-hand))
                  (apply min (alt-ace-hand))))
          (apply < (sort (alt-ace-hand)))) true
   :else false)))

(defn straight-flush? [hand]
  (and (straight? hand)(flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0
   ))

