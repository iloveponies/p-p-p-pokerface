(ns p-p-p-pokerface)

(defn rank [card]
  (let [big-ranks {\T 10,
                   \J 11,
                   \Q 12,
                   \K 13,
                   \A 14}]
    (let [[r _] card]
      (if (Character/isDigit r)
        (Integer/valueOf (str r))
        (get big-ranks r)))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn sames? [hand amount]
  (let [ranks (map rank hand)]
    (if (contains? (clojure.set/map-invert (frequencies ranks)) amount)
      true
      false)))

(defn pair? [hand]
  (sames? hand 2))

(defn three-of-a-kind? [hand]
  (sames? hand 3))

(defn four-of-a-kind? [hand]
  (sames? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (if (== 1 (count (frequencies suits)))
      true
      false)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (if (and
         (contains? (clojure.set/map-invert (frequencies ranks)) 2)
         (contains? (clojure.set/map-invert (frequencies ranks)) 3))
      true
      false)))

(defn two-pairs? [hand]
  (let [freq (frequencies (vals (frequencies (map rank hand))))]
    (if (= 2 (get freq 2))
      true
      false)))

(defn straight? [hand]
  (let [ranks (set (map rank hand))]
    (let [final-ranks
          (if (and (contains? ranks 2) (contains? ranks 4))
            (sort (replace {14 1} ranks))
            (sort ranks))]
      (if (= 5 (count final-ranks))
        (if (= 4 (- (get (vec final-ranks) 4) (get (vec final-ranks) 0)))
          true
          false)
        false))))

(defn straight-flush? [hand]
  (if (and (flush? hand) (straight? hand))
    true
    false))

(defn value [hand]
  )
