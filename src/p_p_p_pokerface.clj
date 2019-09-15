(ns p-p-p-pokerface)

(def repr {"T" 10 "J" 11 "Q" 12 "K" 13 "A" 14})

(defn rank [card]
  (let [r (str (first (seq card)))
        rr (repr r)]
    (if (nil? rr)
      (Integer/valueOf r)
      rr )
    ))

(defn suit [card]
  (let [s (second (seq card))] (str s)))

(defn- n-of-a-kind [hand pred n]
  (let [ranks (map rank hand)
        pairs (filter #(= n %) (-> ranks frequencies vals))]
    (pred (count pairs))
    ))

(defn pair? [hand]
  (n-of-a-kind hand #(> % 0) 2)
)

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand #(> % 0) 3))


(defn four-of-a-kind? [hand]
  (n-of-a-kind hand #(> % 0) 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        s (filter #(= 5 %) (-> suits frequencies vals))]
    (-> s empty? not)
    ))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
   (n-of-a-kind hand #(> % 1) 2) )

(defn straight? [hand]
  (let [ranksa (sort (map rank hand))
        ranksb (sort (map #(if (= 14 %) 1 %) ranksa))
;        _ (println ranksa ranksb)
        straighta (range (apply min ranksa) (+ 1 (apply max ranksa)))
        straightb (range (apply min ranksb) (+ 1 (apply max ranksb)))
;        _ (println straighta straightb)
]
    (or (= ranksa straighta) (= ranksb straightb))
    ))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
   :default 0
))
