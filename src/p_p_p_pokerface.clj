(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        ranks {\T 10
               \J 11
               \Q 12
               \K 13
               \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (ranks r))))


(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn frequencies-by [sel-fn hand]
  (set (vals (frequencies (map sel-fn hand)))))


(defn pair? [hand]
  (contains? (frequencies-by rank hand) 2))

(defn three-of-a-kind? [hand]
 (contains? (frequencies-by rank hand) 3))

(defn four-of-a-kind? [hand]
 (contains? (frequencies-by rank hand) 4))


(defn flush? [hand]
  (contains? (frequencies-by suit hand) 5))

(defn full-house? [hand]
  (let [ranks (frequencies-by rank hand)]
    (and
     (contains? ranks 2)
     (contains? ranks 3))))


(defn two-pairs? [hand]
 (let [freqs (frequencies (vals (frequencies (map rank hand))))]
   (or (= 2 (freqs 2))
       (= 1 (freqs 4)))))

(defn straight? [hand]
  (let [ace-hi-ranks (map rank hand)
        ace-lo-ranks (replace {14 1} ace-hi-ranks)
        min-ace-hi (apply min ace-hi-ranks)
        min-ace-lo (apply min ace-lo-ranks)]
    (and
      (or (= (range min-ace-lo (+ 5 min-ace-lo))
             (sort ace-lo-ranks))
          (= (range min-ace-hi (+ 5 min-ace-hi))
             (sort ace-hi-ranks))))))
     ;(>
     ; (count (keys (frequencies (map suit hand))))
     ; 1))))



(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn high-card? [hand] true)


(defn value [hand]
  (let [valuers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
        (apply max (map (fn [[valuer? value]] (if (valuer? hand) value -1)) valuers))))
