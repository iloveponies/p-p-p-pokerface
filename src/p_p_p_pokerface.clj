(ns p-p-p-pokerface)

(def character-values {\T 10, \J 11, \Q 12, \K 13 \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (character-values r))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-of-a-kind? [hand n]
  (== n (apply max (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (n-of-a-kind? hand 2))


(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn matching-seq? [hand seqn] 
  (= seqn (sort (vals (frequencies (map rank hand))))))

(defn full-house? [hand]
  (matching-seq? hand [2 3]))

(defn two-pairs? [hand]
  (matching-seq? hand [1 2 2]))

(defn straight? [hand]
  (let [working-seq  (sort (map rank hand))
        min_value (apply min working-seq)
        max_value (apply max working-seq)]
    (or (= (range min_value (+ 1 max_value)) working-seq)
        (= (range 1 6) (sort(replace {14 1} working-seq))))))
      
    

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

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
    (high-card? hand) 0))

