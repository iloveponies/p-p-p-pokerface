(ns p-p-p-pokerface)

(defn rank [card]
  (let [c (count card)
        [r _] card
        [r1 r2 _] card]
    (cond
      (== 2 c) (if (Character/isDigit r)
                  (Integer/valueOf (str r))
                  (get {\A 14, \K 13, \Q 12, \J 11 \T 10} r))
      (== 3 c) (Integer/valueOf (str r1 r2)))))

(defn suit [card]
  (str (first (reverse card)))
  )

(defn hand-map [hand]
  (let [card->entry (fn [card] {:card card
                                :classifiers {:rank (rank card),
                                              :suit (suit card)}})]
    (into [] (map card->entry hand))))


(defn hand-property-freq [hand property]
  (let [hm (hand-map hand)]
    (frequencies (map property (map :classifiers hm))))
  )

(defn hand-n-same [hand n]
  (let [n-of-ntuples (get (frequencies (vals (hand-property-freq hand :rank))) n)]
    (if (nil? n-of-ntuples) 0 n-of-ntuples))
  )

(defn pair? [hand]
  (== 1 (hand-n-same hand 2)))

(defn three-of-a-kind? [hand]
  (== 1 (hand-n-same hand 3)))

(defn four-of-a-kind? [hand]
  (== 1 (hand-n-same hand 4)))

(defn flush? [hand]
  (if (== 1 (count (hand-property-freq hand :suit))) true false))

(defn full-house? [hand]
    (if (and (== 1 (hand-n-same hand 3)) (== 1 (hand-n-same hand 2))) true false))

(defn two-pairs? [hand]
  (if (== 2 (hand-n-same hand 2)) true false))

(defn straight? [hand]
    (let [ranks-ordered
          (sort > (map :rank (map :classifiers (hand-map hand))))]
      ;; (if (apply == (- ranks-ordered (range 5))) true false)))
      (if (and (== 5 (count (set ranks-ordered))) (== (- (first ranks-ordered) (last ranks-ordered)) 4))
        true
        (if (and (== 14 (first ranks-ordered)) (= '(5 4 3 2) (rest ranks-ordered))) true false))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [categories #{
                     [true 0]
                     [(pair? hand) 1]
                     [(two-pairs? hand) 2]
                     [(three-of-a-kind? hand) 3]
                     [(straight? hand) 4]
                     [(flush? hand) 5]
                     [(full-house? hand) 6]
                     [(four-of-a-kind? hand) 7]
                     [(straight-flush? hand) 8]}]
    (apply max (map second (filter (fn [x] (= true (first x))) categories)))))
