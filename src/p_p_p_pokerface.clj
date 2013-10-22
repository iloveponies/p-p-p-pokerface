(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _]card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (cond
        (= (str rank) "T") 10
        (= (str rank) "J") 11
        (= (str rank) "Q") 12
        (= (str rank) "K") 13
        (= (str rank) "A") 14
        :else "not a legit rank!"))))


(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (if (<= (count (vals (frequencies ranks))) 4)
      true false)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (if (>= (apply max (vals (frequencies ranks))) 3)
      true false)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (if (>= (apply max (vals (frequencies ranks))) 4)
      true false)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (if (= (count (vals (frequencies suits))) 1)
      true false)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (if (= (sort (vals (frequencies ranks))) (range 2 4))
      true false)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (or (= (sort (vals (frequencies ranks))) (seq [1 2 2]))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (or (= (sort ranks)
           (range (apply min ranks) (+ (apply max ranks) 1)))
        (= (sort (replace {14 1} ranks))
           (range 1 (+ (apply max (replace {14 1} ranks)) 1))))))

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
   :else 0))

;9 26
