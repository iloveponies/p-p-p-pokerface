(ns p-p-p-pokerface)

(defn rank2 [card]
  (let [[r s] card]
    (if (Character/isDigit r) (Integer/valueOf (str r))
      (cond
        (= r \T) 10
        (= r \J) 11
        (= r \Q) 12
        (= r \K) 13
        (= r \A) 14))))

(def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[r s] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (ranks r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand))))
      2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand))))
      3))

(defn four-of-a-kind? [hand]
  (= 4
     (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  ;(println (vals (frequencies (map suit hand))))
  (= 5
     (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (range 2 4)
     (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [signature (sort (vals (frequencies (map rank hand))))]
    ;(println "sign" signature)
    (cond
     (= [1 2 2] signature) true ; doppia coppia
     (= [1 4] signature) true ; poker
     :else
       false)))

(defn straight? [hand]
 (let [signature (sort (map rank hand))
       unique (apply max (vals (frequencies signature))) ; massimo tra i valori forse meglio apply max (map rank hand)
       rng (- (apply max signature) (apply min signature))] ; differenza tra massimo e minimo

   ;(println "h:" hand "s:" signature "u:" unique "r:" rng)
   (if (and (= unique 1) (or (= rng 4) (= rng 12))  ) ; se max-min = 4 o 12 (14-2) e i valori sono unici
     true
     false)))

(defn straight-flush? [hand]
  ;(println "sf")
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        fl (filter (fn [x] ((first x) hand)) checkers)
        va (map second fl)
        mx (apply max va)] ; applico ciascuna fx a hand e prendo solo le true


    (println "h" hand "va" va "mx" mx )
    mx))

