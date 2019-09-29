(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\A 14, \K 13, \Q 12, \J 11, \T 10} rank))))

(defn suit [card]
  (let [[_ rank] card]
    (str rank)))

(defn pair? [hand]
  (let [ranks (map rank hand)
       cards (vals (frequencies ranks))]
   (if (= (apply max cards) 2) true false )))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
       cards (vals (frequencies ranks))]
   (if (= (apply max cards) 3) true false )))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
       cards (vals (frequencies ranks))]
   (if (= (apply max cards) 4) true false )))

(defn flush? [hand]
  (let [suits (map suit hand)]
  (= 5 (first (vals (frequencies suits))))))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
  (= [2 3] (sort (vals (frequencies ranks))))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        numbers (reverse (sort (vals (frequencies ranks))))]
    (cond (= 4 numbers) true
    (= [2 2 1] numbers) true
    :else false )))


(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        replace-low-ace (sort (replace {14 1} ranks))
        low-card (apply min ranks)
        low-card-ace (apply min replace-low-ace)
        ace-flush (range low-card (+ low-card 5))
        low-card-ace-flush (range low-card-ace (+ low-card-ace 5))]

     (or (= ranks ace-flush) (= replace-low-ace low-card-ace-flush))))

(defn straight-flush? [hand]
  (true? (and (straight? hand)
             (flush? hand))))
(defn high-card? [hand]
  true)

(defn value [hand]
  ;not great at all
  (cond (straight-flush? hand)  8
        (four-of-a-kind? hand)  7
        (full-house? hand)      6
        (flush? hand)           5
        (straight? hand)        4
        (three-of-a-kind? hand) 3
        (two-pairs? hand)       2
        (pair? hand)            1
        :else 0))
