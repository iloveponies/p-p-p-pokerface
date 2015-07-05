(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}
        card-rank (ranks r)]
    (if card-rank
      card-rank
      (Integer/valueOf (str r)))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn count-common [hand card-fn]
  (reverse (sort (vals (frequencies (map card-fn hand))))))

(defn count-common-rank [hand]
  (count-common hand rank))

(defn count-common-suit [hand]
  (count-common hand suit))

(defn pair? [hand]
  (>= (first (count-common-rank hand)) 2))

(defn three-of-a-kind? [hand]
  (>= (first (count-common-rank hand)) 3))

(defn four-of-a-kind? [hand]
  (>= (first (count-common-rank hand)) 4))

(defn flush? [hand]
  (>= (first (count-common-suit hand)) 5))

(defn full-house? [hand]
  (= (count-common-rank hand) (seq [3 2])))

(defn two-pairs? [hand]
  (let [[fst snd] (count-common-rank hand)]
    (>= fst snd 2)))

(defn straight? [hand]
  (let [hi-ranks (sort (map rank hand))
        lo-ranks (sort (replace {14 1} hi-ranks))
        hi-start (first hi-ranks)
        lo-start (first lo-ranks)
        hi-target (range hi-start (+ 5 hi-start))
        lo-target (range lo-start (+ 5 lo-start))]
    (or (= hi-ranks hi-target)
        (= lo-ranks lo-target))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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
