(ns p-p-p-pokerface)

(defn any [bools]
  (loop [b bools]
    (if (empty? b)
      false
      (if (first b)
        true
        (recur (rest b))))))

(declare four-of-a-kind?)

(defn rank [card]
  (let [[fst snd] card]
    (case fst
      \T 10
      \J 11
      \Q 12
      \K 13
      \A 14
      (Integer/valueOf (str fst)))))

(defn freq-filter [n]
  (fn [x] (= (second x) n)))

(defn contains-hand-freq? [n hand]
  (any (map (freq-filter n)
            (frequencies (map rank hand)))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (contains-hand-freq? 2 hand)) 

(defn two-pairs? [hand]
  (or (= 2 (count (filter (freq-filter 2)
                          (frequencies (map rank hand)))))
      (four-of-a-kind? hand)))

(defn three-of-a-kind? [hand]
  (contains-hand-freq? 3 hand))

(defn four-of-a-kind? [hand]
  (contains-hand-freq? 4 hand))


(defn straight? [hand]
  (let [sorted1 (sort (map rank hand))
        sorted2 (sort (map (fn [x] (if (= 14 (rank x)) 1 (rank x))) hand))
        straight?? (fn [x] (and (apply < x)
                                (= (first x)
                                   (- (last x)
                                      4))))]
    (or
     (straight?? sorted1)
     (straight?? sorted2))))

(defn flush? [hand]
  (apply = (map second hand)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
