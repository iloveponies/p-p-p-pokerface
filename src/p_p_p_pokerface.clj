(ns p-p-p-pokerface)

(defn rank [[r s]]
  (cond
    (= r \T) 10
    (= r \J) 11
    (= r \Q) 12
    (= r \K) 13
    (= r \A) 14
    :else (Integer/valueOf (str r))))

(defn suit [[r s]]
  (str s))

(defn max-same-rank [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (= 2 (max-same-rank hand)))

(defn three-of-a-kind? [hand]
  (= 3 (max-same-rank hand)))

(defn four-of-a-kind? [hand]
  (= 4 (max-same-rank hand)))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [frqs (vals (frequencies (map rank hand)))]
    (and (.contains frqs 2)
         (.contains frqs 3))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (let [fs (vals (frequencies (map rank hand)))
            sfs (sort (filter pos? fs))]
        (= sfs [1 2 2])))) ; magic constant: two pairs consists of 1, 2 and 2 cards of same rank

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        fst (first sorted)
        straight-from-first (range fst (+ fst 5))]
    (or (= sorted straight-from-first)
        (= sorted [2 3 4 5 14])))) ; allow for ace

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [checkers #{[pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3] 
                   [straight? 4]   
                   [flush? 5] 
                   [full-house? 6] 
                   [four-of-a-kind? 7]  
                   [straight-flush? 8]}
        getval (fn [[c v]] 
                 (if (c hand) 
                   v 
                   0))
        hand-values (map getval checkers)]
    (apply max hand-values)))
