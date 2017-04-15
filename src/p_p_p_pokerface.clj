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
  (let [ranks (sort (map rank hand))
        suits (set (map suit hand))]
    (and (apply < ranks)
         (== 1 (count suits)))))

(defn full-house? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (and (== 2 (first freqs))
         (== 3 (second freqs)))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (let [freqs (sort (vals (frequencies (map rank hand))))
            twos (filter (fn [x] (== x 2)) freqs)
            num-twos (count twos)]
        (== 2 num-twos))))

(defn straight? [hand]
  (let [highest (apply max (map rank hand))
        cards (sort (map rank hand))
        lowest (apply min (map rank hand))]
    (if
      (= highest 14)
      (cond
       (= (range 10 15) cards) true
       (= [2 3 4 5 14] cards) true
       :else false)
      (= (range lowest (+ lowest 5)) cards)))
  )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
   :else 0))

