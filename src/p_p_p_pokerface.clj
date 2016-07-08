(ns p-p-p-pokerface)

(defn rank [card] (let [[fst snd] card]
   (if(Character/isDigit fst) (Integer/valueOf (str fst)) (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst)  )))

(defn suit [card] (let [[fst snd] card]
   (str snd)))

(defn pair? [hand]
    (if (= 2 (some #{2} (vals (frequencies (map rank hand)))) ) true false))

(defn three-of-a-kind? [hand]
  (if (= 3 (some #{3} (vals (frequencies (map rank hand)))) ) true false))

(defn four-of-a-kind? [hand]
  (if (= 4 (some #{4} (vals (frequencies (map rank hand)))) ) true false))

(defn flush? [hand]
  (if (= 5 (some #{5} (vals (frequencies (map suit hand)))) ) true false))

(defn full-house? [hand]
  (if (and (three-of-a-kind? hand) (pair? hand)) true false))

(defn parillisetfrekvenssit [hand]
      (filter (fn [freq] (if (= 0 (mod freq 2)) true false ) )
              (vals (frequencies (map rank hand))) )
  )

(defn two-pairs? [hand]
  (if

    (or
      (and (boolean (first  (parillisetfrekvenssit hand))) (<= 4 (first (parillisetfrekvenssit hand) )) ) (<= 2 (count (parillisetfrekvenssit hand)) ))

    true false)

  )


(defn increasing [x y]
  (if (or (= y nil) (= (+ x 1) y) (= (- x 1) y)) true false))


(defn contains-duplicates? [a-seq] (if (= (count a-seq) (count (into #{} a-seq) )) false true) )

(defn add_ykkonen [rankit]
  (if (and (contains? rankit 14) (contains? rankit 2)) (conj (disj rankit 14) 1) rankit))

(defn straight? [hand]
   (let [rankit (map rank hand) lista (sort (add_ykkonen (set rankit))) mapatty (map increasing lista (rest lista) )]

     (if (and (= (contains-duplicates? rankit) false) (apply = mapatty) (= (first mapatty) true))  true false )))


(defn high-card? [hand]
  true)

(defn straight-flush? [hand] (if (and (straight? hand) (flush? hand)) true false))


(defn high-card? [hand]
  true)

(defn value [hand]
 ( let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}  checker1 [high-card? 1]]

   (apply max (map second (filter (fn [checker] (if ((first checker) hand) checker false ) )
              checkers) ))

    ))
