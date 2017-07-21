(ns p-p-p-pokerface)

(defn replacements [char]
  (let [cards {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (get cards char))
)


(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (Integer/valueOf (str (replacements rank))))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand) values (vals(frequencies ranks))]
    (if(= (apply max values) 2)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand) values (vals(frequencies ranks))]
    (if(= (apply max values) 3)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand) values (vals(frequencies ranks))]
    (if(= (apply max values) 4)
      true
      false)))

(defn flush? [hand]
  (let [suits (map suit hand) values (vals(frequencies suits))]
    (if(= (apply max values) 5)
      true
      false)))

(defn full-house? [hand]
  (let [ranks (map rank hand) values (vals(frequencies ranks))]
    (if(= (sort values) (range 2 4))
      true
      false)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand) values (vals(frequencies ranks))]
    (if(= (sort values) (sort (conj (range 1 3) 2)))
      true
      false)))

(defn straight? [hand]
  (let [ranks (sort(map rank hand)) ranks2 (sort(replace {14 1} ranks)) [first] ranks [first2] ranks2]
    (or (= ranks (range first (+ first 5))) (= ranks2 (range first2 (+ first2 5))))))

(defn straight-flush? [hand]
  (cond
    (and (straight? hand) (flush? hand)) true
    :else false))

(defn high-card? [hand]
  true)

(defn value [hand]
 (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        func (fn [x] (if ((first x) hand) (second x) -1))]
    (apply max (map func checkers))))
