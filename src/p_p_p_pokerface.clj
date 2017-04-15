(ns p-p-p-pokerface)

(def values {\T 10 \J 11 \Q 12
             \K 13 \A 14})

(defn rank [card]
  (let [[fst _] card]
    (cond
      (Character/isDigit fst)
     (Integer/valueOf (str fst))
     :else
     (get values fst))))



(defn suit [card]
 (let [[_ snd] card]
   (str snd)))

(defn largest-combination [hand]
  (let [ranks
        (map rank hand)]
    (apply max
           (vals (frequencies ranks)))))

(defn largest-combination-suits [hand]
  (let [suits
        (map suit hand)]
    (apply max
           (vals (frequencies suits)))))



(defn pair? [hand]
  (== (largest-combination hand) 2))


(defn three-of-a-kind? [hand]
  (== (largest-combination hand) 3))


(defn four-of-a-kind? [hand]
  (== (largest-combination hand) 4))

(defn flush? [hand]
  (== (largest-combination-suits hand) 5))

(defn full-house? [hand]
  (let [ranks
        (map rank hand)
        freqs (frequencies ranks)
        values (vals freqs)
        sorted-values (sort values)]
    (and
     (==
      (first sorted-values) 2)
     (==
      (second sorted-values) 3))))


(defn two-pairs? [hand]
  (let [ranks
        (map rank hand)
        freqs (frequencies ranks)
        values (vals freqs)
        sorted-values (reverse (sort values))]
    (and
     (==
      (first sorted-values) 2)
     (==
      (second sorted-values) 2))))


(defn straight? [hand]
  (let [ranks
        (map rank hand)
        sorted-ranks (sort ranks)
        first-value (first sorted-ranks)
        straight (range
                  first-value
                  (+ 5 first-value))]
    (or
     (= straight sorted-ranks)
     (= [1 2 3 4 5]
        (sort (replace {14 1} sorted-ranks))))))



(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

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







