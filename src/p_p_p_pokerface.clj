(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (get {\A 14, \K 13, \Q 12, \J 11, \T 10} fst))))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (let [values (vals (frequencies (map rank hand)))]
  (and
   (= (count values) 4)
   (= (apply max values) 2))))

(defn three-of-a-kind? [hand]
  (let [values (vals (frequencies (map rank hand)))]
  (and
   (= (count values) 3)
   (= (apply max values) 3))))

(defn four-of-a-kind? [hand]
  (let [values (vals (frequencies (map rank hand)))]
  (and
   (= (count values) 2)
   (= (apply max values) 4))))

(defn flush? [hand]
  (let [suits (vals (frequencies (map suit hand)))]
    (= (count suits) 1)))

(defn full-house? [hand]
  (let [values (vals (frequencies (map rank hand)))]
  (and
   (= (count values) 2)
   (= (apply max values) 3))))

(defn two-pairs? [hand]
  (let [values (sort (vals (frequencies (map rank hand))))]
  (or (and
   (= (count values) 3)
       (= (apply max values) 2)
       (= (apply min values) 1))
      (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [values (sort (map rank hand))
        ace-values (sort (replace {14 1} values))
        minim (apply min values)]
    (or (= values (range minim (+ minim  5)))
        (= ace-values (range 1 6)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[true 0]  [(pair? hand) 1]
                 [(two-pairs? hand) 2]  [(three-of-a-kind? hand) 3]
                 [(straight? hand) 4]   [(flush? hand) 5]
                 [(full-house? hand) 6] [(four-of-a-kind? hand) 7]
                 [(straight-flush? hand) 8]}
        true-list (filter first checkers)
        list (map second true-list)]
    (apply max list)))
