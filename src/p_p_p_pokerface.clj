(ns p-p-p-pokerface)

(defn suit [card]
  (let [[_ s] card]
    (str s)))


(def rank-replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[r _] card]
    (cond
     (Character/isDigit r) (Integer/valueOf (str r))
     :else (rank-replacements r))))


(defn pair? [hand]
  (== 2
      (apply max
             (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3
      (apply max
             (vals (frequencies (map rank hand))))))


(defn four-of-a-kind? [hand]
  (== 4
      (apply max
             (vals (frequencies (map rank hand))))))


(defn flush? [hand]
  (== 5
      (apply max
       (vals (frequencies (map suit hand))))))


(defn full-house? [hand]
  (let [freqs (frequencies (map rank hand))]
    (and (== 2
             (count freqs)
             (first (sort (vals freqs)))))))

(defn two-pairs? [hand]
  (let [freqs (frequencies (map rank hand))]
    (and (== 3
             (count freqs))
         (== 2
             (first (keys (frequencies (vals freqs))))))))

;; this turned out quite verbose. maybe there is a more elegant solution
(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        contains-number? (fn [lst nbr] (not (== -1 (.indexOf lst nbr))))
        five-successive-ranks? (fn [ranks] 
                                 (let [rank-set (apply sorted-set ranks)]
                                   (and (== 5 (count rank-set))
                                        (== 4 (- (apply max rank-set) (apply min rank-set))))))
        replace-possible-ace-rank (fn [sranks] (cond 
                                                (contains-number? sranks 14) (sort (conj (vec (take 4 sranks)) 1))
                                                :else sranks))]
    (cond
     (five-successive-ranks? sorted-ranks) true
     (five-successive-ranks? (replace-possible-ace-rank sorted-ranks)) true
     :else false)))
        

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

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

