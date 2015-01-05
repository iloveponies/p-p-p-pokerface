(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        ranks {\T 10,
               \J 11
               \Q 12
               \K 13
               \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get ranks fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-of-a-kind? [hand n]
  (>=
    (apply
      max
      (vals
        (frequencies
          (map rank hand))))
    n))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (=
   (sort
     (vals
       (frequencies
         (map rank hand))))
   [2 3]))

(defn two-pairs? [hand]
  (=
   (sort
     (vals
       (frequencies
         (map rank hand))))
   [1 2 2]))

(defn strict-straight? [ranks]
  (let [card-seq
        (sort ranks)]
    (=
     card-seq
     (range (first card-seq)
            (+ 1 (last card-seq))))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (or
      (strict-straight? ranks)
      (strict-straight? (replace {14 1} ranks)))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max
           (map second
                (filter (fn [[checker value]]
                          (checker hand))
                        checkers)))))
