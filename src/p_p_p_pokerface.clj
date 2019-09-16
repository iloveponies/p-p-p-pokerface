(ns p-p-p-pokerface)

;; ex 1
(defn suit [card]
  (let [[_ snd] card]

    (str snd)
  )
)



;;ex2
(def replacements {\T 10, \J 11, \Q 12 , \K 13 , \A 14})


(defn rank [card]

   (let [[fst _] card]

     (cond
      (Character/isDigit fst) (Integer/valueOf (str fst))
      :else (replacements fst)
      )
    )
  )


;;ex3
(defn pair? [hand]
  (let [ranks (map rank hand)]
    (== 2(apply max (vals (frequencies ranks))))
    )
  )

;;ex 4
(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]

  (== 3 (apply max (vals (frequencies ranks))))))




(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]

  (== 4 (apply max (vals (frequencies ranks))))))


(defn flush? [hand]
 (let [suits (map suit hand)]

  (== 5 (apply max (vals (frequencies suits))))))

;; ex 7
(defn full-house? [hand]
   (let [ranks (map rank hand)]
    ( =
      (sort(vals (frequencies ranks)))
       (range 2 4) ;;=> (2 3)
    )
   )
  )

;; ex 8
(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (sort(vals (frequencies ranks)))]

     (or (= (seq [1 2 2]) freqs) (= (seq [1 4]) freqs))))

;;ex 9
(defn straight? [hand]
  (let [srank1 (sort  (map rank hand))
        srank2 (sort(replace {14 1} srank1 ))
        m1 (apply min srank1)
        m2 (apply min  srank2)]
    (or
     ;; case \A = 14
     (= (range m1 (+ m1 5)) srank1)
     ;; case \A = 1
     (= (range m2 (+ m2 5)) srank2)
     )
    )

  )

(defn straight-flush? [hand]
  (and (straight? hand)(flush? hand))
  )

(defn value [hand]
  (let [

        high-card? (fn [hand] true)
        checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
     (apply max

            (map second
            (filter (fn [vec-matching]
                      (let [pred? (first vec-matching)] (pred? hand)))
             checkers)
             )
      )
    )
  )

