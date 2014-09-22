(ns p-p-p-pokerface)

(defn rank [card]
  (def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[rank _] card]
   (println (Character/isDigit rank))
   (cond (Character/isDigit rank) (Integer/valueOf (str rank))
         :else (get ranks rank))))


(defn suit [card]
  (let [[_ suit] card]
   (str suit)))

(defn pair? [hand]
  (let [freq (frequencies (vals (frequencies (map rank hand))))]
    (and (= 3 (get freq 1)) (= 1 (get freq 2)))))

(defn three-of-a-kind? [hand]
  (let [freq (frequencies (vals (frequencies (map rank hand))))]
    (and (= 2 (get freq 1)) (= 1 (get freq 3)))))

(defn four-of-a-kind? [hand]
  (let [freq (frequencies (vals (frequencies (map rank hand))))]
    (and (= 1 (get freq 1)) (= 1 (get freq 4)))))

(defn flush? [hand]
  (let [freq (frequencies (vals (frequencies (map suit hand))))]
    (= 1 (get freq 5))))

(defn full-house? [hand]
  (let [freq (frequencies (vals (frequencies (map rank hand))))]
    (and (= 1 (get freq 3)) (= 1 (get freq 2)))))

(defn two-pairs? [hand]
  (let [freq (frequencies (vals (frequencies (map rank hand))))]
    (and (= 1 (get freq 1)) (= 2 (get freq 2)))))

(defn straight? [hand]
  (let [sortedhand (sort (map rank hand))
        handmax (apply max sortedhand)
        handmin (apply min sortedhand)
        checkhigh (= sortedhand (range handmin (+ 1 handmax)))
        checklow (= sortedhand [2 3 4 5 14])]
    (or checklow checkhigh)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))



(defn value [hand]
  (defn high-card? [hand]
    true)
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matching-hands (filter (fn [x] ((first x) hand)) checkers)]
    (apply max (map second matching-hands))))
