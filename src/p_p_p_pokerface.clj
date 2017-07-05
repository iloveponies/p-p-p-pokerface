(ns p-p-p-pokerface)

(defn rank [card]
  (let [[arvo] card]
    (if (Character/isDigit arvo)
      (Integer/valueOf (str arvo))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} arvo))))

(defn suit [card]
  (let [[_ maa] card] 
    (str maa)))

(defn pair? [hand]
  (let [values (mapv rank hand)]
    (if (<= 2 (apply max (vals (frequencies values))))
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [values (mapv rank hand)]
    (if (<= 3 (apply max (vals (frequencies values))))
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [values (mapv rank hand)]
    (if (<= 4 (apply max (vals (frequencies values))))
      true
      false)))

(defn flush? [hand]
  (apply = (mapv suit hand)))

(defn full-house? [hand]
  (let [abs-pair? (fn [hd] (let [values (mapv rank hd)]
    (if (= 2 (apply max (filter
                          (fn [val] (if (< 2 val)
                                      false
                                      true))
                            (vals (frequencies values)))))
      true
      false)))]
  (if (and (abs-pair? hand) (three-of-a-kind? hand))
    true
    false
  )))

(defn two-pairs? [hand]
  (let [values (mapv rank hand)]
    (if (or 
          (and (= 2 (apply max (vals (frequencies values)))) 
               (= 2 (count (filter (fn [val] (if (< 1 val)
                                               true
                                               false)) (vals (frequencies values))))))
          
            (= 4 (apply max (vals (frequencies values)))))
      true
      false)
  )
    )


(defn straight? [hand]
  (let [lower-ace-vector (fn[hnd]
                           (filterv (fn [vct] (if (= 14 vct)
                                                false
                                                true))
                                    (conj  (mapv rank hnd) 1)))]
  (let [values (if (and (= 14 (apply max (mapv rank hand) ))
                        (> 7 (apply min (mapv rank hand))))
                 (lower-ace-vector hand)
                 (mapv rank hand)
                 )]
    (if (and 
          (= 5 (count (filter (fn [val] (if (= 1 val)
                                               true
                                               false)) (vals (frequencies values)))))
          (= 4 (-
                 (apply max values)
                 (apply min values))))
      true
      false)))
)
(defn straight-flush? [hand]
  (if (and
        (straight? hand)
        (flush? hand))
    true
    false))

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
