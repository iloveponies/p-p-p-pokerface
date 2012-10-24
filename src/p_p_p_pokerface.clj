(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _ ] card]
    (cond
      (= fst \T) 10
      (= fst \J) 11
      (= fst \Q) 12
      (= fst \K) 13
      (= fst \A) 14
      :else (Integer/valueOf (str fst)))))

(defn suit [card]
  (let [[_ snd ] card]
    (str snd)))

(defn pair? [hand]
  (cond 
    (< 1 (apply max (vals (frequencies (map rank hand))))) true
    :else false))

(defn three-of-a-kind? [hand]
  (cond 
    (< 2 (apply max (vals (frequencies (map rank hand))))) true
    :else false))

(defn four-of-a-kind? [hand]
  (cond 
    (< 3 (apply max (vals (frequencies (map rank hand))))) true
    :else false))

(defn flush? [hand]
  (cond 
    (< 4 (apply max (vals (frequencies (map suit hand))))) true
    :else false))

(defn full-house? [hand]
  (let [handfreq (vals (frequencies (map rank hand)))]
    (cond
      (and (= 3 (first handfreq))
           (= 2 (count handfreq))) 
        true
      :else false)))

(defn two-pairs? [hand]
  (let [handfreq (vals (frequencies (map rank hand)))]
    (cond
      (= (first handfreq) 
         (second handfreq)
         2) 
        true 
      (four-of-a-kind? hand) true
      :else false)))

(defn straight? [hand]
  (let [ranklist (map 
                   (fn [x] (Integer/valueOf (str x))) 
                   (sort (map rank hand)))
        ; case of ace?  i considered this to be the most readable solution.
        rankaces (sort (replace {14 1} ranklist))
        rankfreq (vals (frequencies ranklist))]
    (cond
      (and
        (= (count rankfreq) 5)
        (or
          (= 4 (- (last ranklist) (first ranklist)))
          (= 4 (- (last rankaces) (first rankaces))))) true
      :else false)))

(defn straight-flush? [hand]
  (cond
    (and (flush? hand)
         (straight? hand)) true
    :else false))

(defn value [hand]
  ; why not like this?  boring and static?
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


