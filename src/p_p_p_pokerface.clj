(ns p-p-p-pokerface)

    (defn rank [card]
       (let [[value _] card]
            (let [values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
                   (if (Character/isDigit value)
                            (Integer/valueOf (str value))
                                    (get values value)))))

    (defn suit [card]
       (let [[_ snd] card]
            (str snd)))

    (defn pair? [hand]
       (if (> 2 (apply max (vals (frequencies (map rank hand))))) false true))

    (defn three-of-a-kind? [hand]
       (if (> 3 (apply max (vals (frequencies (map rank hand))))) false true))

    (defn four-of-a-kind? [hand]
       (if (> 4 (apply max (vals (frequencies (map rank hand))))) false true))

    (defn flush? [hand]
       (apply = (map suit hand)))

    (defn full-house? [hand]
       (= (range 2 4) (sort (vals (frequencies (map rank hand))))))

    (defn two-pairs? [hand]
       (let [hand-seq (sort (vals (frequencies (map rank hand))))]
            (cond
                  (= (seq [5]) hand-seq) true
                       (= (seq [1 4]) hand-seq) true
                            (= (seq [2 3]) hand-seq) true
                                 (= (seq [1 2 2]) hand-seq) true
                                      :else false)))

    (defn straight? [hand]
       (let [ranks (map rank hand)]
            (let [sorted-ranks (sort (if (= 2 (apply min ranks)) (replace {14 1} ranks) ranks))]
                   (= sorted-ranks 
                             (range (apply min sorted-ranks) (+ 1 (apply max sorted-ranks)))))))

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

