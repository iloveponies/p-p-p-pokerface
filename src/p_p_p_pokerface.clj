(ns p-p-p-pokerface)

(defn rank [[rg st]]
  (if (Character/isDigit rg)
    (Integer/valueOf (str rg))
    (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rg)))

(defn suit [card]
  (-> card second str))

(defn n-of-a-kind? [hand n]
  (contains? (set (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
   (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= (get (frequencies (vals (frequencies (map rank hand)))) 2) 2))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        low (first ranks)
        high (last ranks)
        ]
    (and ;(> (count (frequencies (map suit hand)))
         ;   1)
         (or (= (range low (inc high)) ranks)
             (= (range 1 6) (sort (replace {14 1} ranks)))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [check] ((first check) hand)) checkers)))))
