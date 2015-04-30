(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (cond
      (Character/isDigit r) (Integer/valueOf (str r)) 
      (= \T r) 10
      (= \J r) 11
      (= \Q r) 12
      (= \K r) 13
      (= \A r) 14)))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn how-many? [hand n]
  (let [values (vals (frequencies (map rank hand)))]
    (= (apply max values) n)))

(defn pair? [hand]
  (how-many? hand 2))

(defn three-of-a-kind? [hand]
  (how-many? hand 3))

(defn four-of-a-kind? [hand]
  (how-many? hand 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (= (sort values) (seq [2 3]))))

(defn two-pairs? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (= (sort values) (seq [1 2 2]))))

(defn pos-range [ranks]
  (let [low (apply min ranks)]
    (range low (+ minrank 5))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        A1 (sort (replace {1 14} ranks))
        A14 (sort (replace {14 1} ranks))]
        (or (= A1 (pos-range A1))
            (= A14 (pos-range A14)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))