(ns p-p-p-pokerface)

(def replacements { \T 10
                    \J 11 
                    \Q 12 
                    \K 13 
                    \A 14 })

(defn rank [card]
  (let [[rnk s] card]
    (cond (Character/isDigit rnk) (Integer/valueOf (str rnk))
    :else                         (replacements rnk)))
)
(defn suit [card]
  (let [[rnk s] card]
    (str s))
)

(defn pair? [hand]
  (let [handranks (vec (map rank hand))]
    (let [valfqs (vec (vals (frequencies handranks)))]
      (cond (= (apply max valfqs ) 2) true
            :else false))) 
)

(defn three-of-a-kind? [hand]
  (let [handranks (vec (map rank hand))]
    (let [valfqs (vec (vals (frequencies handranks)))]
      (cond (= (apply max valfqs ) 3) true
            :else false))) 
)

(defn four-of-a-kind? [hand]
  (let [handranks (vec (map rank hand))]
    (let [valfqs (vec (vals (frequencies handranks)))]
      (cond (= (apply max valfqs ) 4) true
            :else false)))
)

(defn flush? [hand]
  (let [handsuits (vec (map suit hand))]
    (let [keycount (vec (keys (frequencies handsuits)))]
      (cond (= (count keycount ) 1) true
            :else false)))
  )

(defn full-house? [hand]
  (let [handranks (vec (map rank hand))]
    (let [valfqs (sort (vec (vals (frequencies handranks))))]
      (if (= valfqs [2 3])
        true
        false)))
)

(defn two-pairs? [hand]
  (let [handranks (vec (map rank hand))]
    (let [valfqs (sort (vec (vals (frequencies handranks))))]
      (if (= valfqs [1 2 2])
        true
        false)))
)
(defn handranks [hand] 
  (sort (vec (map rank hand))))

(defn minmaxdif [hand] 
  (let [ranks (handranks hand)]
    (- (apply max ranks) (apply min ranks))))

(defn valfqs [hand]
  (let [ranks (handranks hand)]
    (sort (vec (vals (frequencies ranks))))))

(defn acereplaced [hand]
  (sort (replace {14 1} (handranks hand))))

(defn straight? [hand]
  (let [rephand (acereplaced hand)]
    (cond (and (= 4 (minmaxdif hand)) 
               (= 5 (count (vals (frequencies hand))))
               (< (handranks hand))) true
          (and (= 4 (minmaxdif rephand))
               (= 5 (count (valfqs rephand))) 
               (< (handranks rephand))) true
          :else   false)))


(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
