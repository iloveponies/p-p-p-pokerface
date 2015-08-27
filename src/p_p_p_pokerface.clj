(ns p-p-p-pokerface)

(def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (ranks fst))))

(defn suit [card]
    (let [[_ snd] card]
      (str snd)))

(defn pair? [hand]
  (let [samoja (apply max (vals (frequencies (map rank hand))))]
  (if (or (= 2 samoja)
          (= 3 samoja)
          (= 4 samoja)
          (= 5 samoja))
          true false)))

(defn three-of-a-kind? [hand]
  (let [samoja (apply max (vals (frequencies (map rank hand))))]
  (if (or (= 3 samoja)
          (= 4 samoja)
          (= 5 samoja))
          true false)))

(defn four-of-a-kind? [hand]
  (let [samoja (apply max (vals (frequencies (map rank hand))))]
  (if (or (= 4 samoja)
          (= 5 samoja))
          true false)))

(defn flush? [hand]
  (let [samoja-vareja (apply max (vals (frequencies (map suit hand))))]
    (if (= 5 samoja-vareja) true false)))

(defn full-house? [hand]
  (let [samoja-max (apply max (vals (frequencies (map rank hand))))]
    (let [samoja-min (apply min (vals (frequencies (map rank hand))))]
      (if (and (= 3 samoja-max)
               (= 2 samoja-min)) true false))))

(defn two-pairs? [hand]
  (if (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2)) true false))

(defn straight? [hand]
  (let [pienin-kortti (apply min (keys (frequencies (map rank hand))))]
    (let [suurin-kortti (apply max (keys (frequencies (map rank hand))))]
      (if (not= 1 (apply max (vals (frequencies (map rank hand))))) false
        (if (= 4 (- suurin-kortti pienin-kortti)) true
         (if (= 14 suurin-kortti)
          (if (= 5 (apply max (keys (frequencies (replace {suurin-kortti 1} (map rank hand)))))) true false)
            ))))))

(defn straight-flush? [hand]
  (if (and (straight? hand)
           (flush? hand))
    true false))


(defn value [hand]
  (if (straight-flush? hand) 8
    (if (four-of-a-kind? hand) 7
      (if (full-house? hand) 6
        (if (flush? hand) 5
          (if (straight? hand) 4
            (if (three-of-a-kind? hand) 3
              (if (two-pairs? hand) 2
                (if (pair? hand) 1
                  0)))))))))





