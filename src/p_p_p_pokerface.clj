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


(defn straight? [hand]
  (let [minimi (apply min (handranks hand))
        rephand (acereplaced hand) 
        handrange (range minimi (+ minimi 5))
        sorthandranks (sort (map rank hand))]
      (if (= sorthandranks handrange) true
          (if (= (sort (replace {14 1} sorthandranks)) (range 1 (+ 1 5))) true
            false))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
