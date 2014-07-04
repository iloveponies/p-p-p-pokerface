(ns p-p-p-pokerface)

(defn rank [card]
  (let [card-rank (first card)
        char-to-int #(Integer/valueOf (str %))
        alpha-ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit card-rank)
      (char-to-int card-rank)
      (char-to-int (get alpha-ranks card-rank)))))

(defn suit [card]
  (str (second card)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 
               2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 
               3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 
               4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== (first (vals (frequencies suits)))
        5)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (== (count (filter #(== % 2) (vals (frequencies ranks)))) 
        2)))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        low (apply min sorted-ranks)
        high (apply max sorted-ranks)]
    (if (== high 14)
      (or (= (sort (replace {high 1} sorted-ranks)) (range 1 6)) 
          (= sorted-ranks (range 10 15)))
      (= sorted-ranks (range low (+ low 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [hand-vals #{[high-card? 0]  [pair? 1]
                    [two-pairs? 2]  [three-of-a-kind? 3]
                    [straight? 4]   [flush? 5]
                    [full-house? 6] [four-of-a-kind? 7]
                    [straight-flush? 8]}
        checker #(if ((first %) hand) (second %) 0)]
    (apply max (map checker hand-vals))))
