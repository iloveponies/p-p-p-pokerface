(ns p-p-p-pokerface)

(defn rank [card]
  (let
    [[frst _] card]
    (if
      (Character/isDigit frst)
      (Integer/valueOf (str frst))
      (cond
       (= frst \T) 10
       (= frst \J) 11
       (= frst \Q) 12
       (= frst \K) 13
       (= frst \A) 14
       :else 0))))

(defn suit [card]
  (let
    [[_ second] card]
    (str second)))

(defn pair? [hand]
    (let
      [ranks (map rank hand)]
      (< 1 (apply max (vals (frequencies ranks))))))

(defn three-of-a-kind? [hand]
  (let
      [ranks (map rank hand)]
      (< 2 (apply max (vals (frequencies ranks))))))

(defn four-of-a-kind? [hand]
  (let
      [ranks (map rank hand)]
      (< 3 (apply max (vals (frequencies ranks))))))

(defn flush? [hand]
  (let
    [suits (map suit hand)]
    (== 1 (count (frequencies suits)))))

(defn full-house? [hand]
  (let
    [ranks (sort (vals (frequencies (map rank hand))))]
    (and (== (first ranks) 2) (== (last ranks) 3))))

(defn two-pairs? [hand]
  (let
    [ranks (sort (vals (frequencies (map rank hand))))]
    (or (full-house? hand) (== 4 (first ranks))
        (and (== 1 (first ranks)) (== 3 (count ranks)) (== 2 (last ranks))))))

(defn straight? [hand]
  (let
    [ranks (sort (vals (frequencies (map rank hand))))
     cards (sort (map rank hand))]
    (and (== 5 (count ranks))
         (if (== 14 (last cards))
           (or (== 10 (first cards))
               (== 5 (last (sort (replace {14 1} cards)))))
           (== 4 (- (last cards) (first cards)))))))

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
