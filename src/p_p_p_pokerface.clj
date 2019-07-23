(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[r s] card]
  (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get replacements r))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
     (apply = suits)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        fullh (sort (vals (frequencies ranks)))]
     (= fullh, [2 3])))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        twop (sort (vals (frequencies ranks)))]
     (= twop, [1 2 2])))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (if (not (= ranks (range (first ranks) (+ (first ranks) 5))))
        ( = (range 1 6)  (sort (replace {14 1} ranks)) )
        true)))

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
