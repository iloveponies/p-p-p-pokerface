(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (if (<= 2 (apply max (vals freqs)))
     true
     false)))


(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (if (<= 3 (apply max (vals freqs)))
     true
     false)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (if (<= 4 (apply max (vals freqs)))
     true
     false)))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (frequencies suits)]
    (if (= 5 (apply max (vals freqs)))
      true
      false)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (if (= (sort (vals freqs)) [2 3])
      true
      false)))

(defn two-pairs? [hand]
   (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (if (= (sort (vals freqs)) [1 2 2])
      true
      false)))

(defn straight? [hand]
  (let [values (map rank hand)
        high (apply max values)]
    (if (= (sort values) (range (- high 4) (+ high 1)))
      true
      (if (= (sort (replace {14 1} values)) (range 1 6))
       true
       false))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (if (straight-flush? hand)
    8
    (if (four-of-a-kind? hand)
      7
      (if (full-house? hand)
        6
        (if (flush? hand)
          5
          (if (straight? hand)
            4
            (if (three-of-a-kind? hand)
              3
              (if (two-pairs? hand)
                2
                (if (pair? hand)
                  1
                  0)))))))))

