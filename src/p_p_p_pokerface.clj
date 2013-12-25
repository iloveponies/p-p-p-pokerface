(ns p-p-p-pokerface)

(defn do-a-thing [x]

  (let [y (+ x x)]
    (Math/pow y y)))


(defn suit [card]
  (let [[_ st] card]
    (str st)))


(defn rank [card]
  (let [[rnk _] card
        xln-map {\T "10" \J "11" \Q "12" \K "13" \A "14"}]

    (Integer/valueOf
     (if (Character/isDigit rnk)
      (str rnk)
      (str (get xln-map rnk))))))

(defn number-of-a-kind-in-hand
      [num hand]
  (->> hand
       (map rank)
       (frequencies)
       (map second)
       (filter #(== num %))
       (count)))


(defn pair? [hand]
    (> (number-of-a-kind-in-hand 2 hand) 0))




(defn three-of-a-kind? [hand]
  (== 1 (number-of-a-kind-in-hand 3 hand)))

(defn four-of-a-kind? [hand]
  (== 1 (number-of-a-kind-in-hand 4 hand)))

(defn flush? [hand]
  (>
          (->> hand
          (map suit)
          (frequencies)
          (map second)
          (filter #(== % 5))
          count)
     0)
  )

(defn full-house? [hand]
  (and
     (three-of-a-kind? hand)
     (pair? hand)))

(defn two-pairs? [hand]
  (== 2 (number-of-a-kind-in-hand 2 hand)))

(defn straight? [hand]
  (let [ sorted (->> hand (map rank) (sort))
        lb (apply min sorted)]

     (or
       (= sorted [2 3 4 5 14])  ;low-ace straight scenario
       (= sorted (range lb (+ lb 5))))))

(defn straight-flush? [hand]
  (and
     (straight? hand)
     (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]

 (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2]
                  [three-of-a-kind? 3] [straight? 4] [flush? 5]
                  [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}]

   ;alternative version
   ;(->> checkers  (filter #((first %) hand)) (map second) (apply max))

   (apply max (map second (filter #((first %) hand) checkers)))))

(comment
  (defn count-freq
          [num coll]
    (count (filter #(== % num) coll)))


(defn pair? [hand]
  (reduce #(or %1 %2)
          (map #(or (== 2 %))
               (map second
                    (frequencies
(defn count-freq
  [num coll]
  (count (filter #(== % num) coll)))
                     (map rank hand))))))


  (count
   (filter
    #(== 3 %)
    (map second
         (frequencies
          (map rank three-of-a-kind-hand)))))
  )


