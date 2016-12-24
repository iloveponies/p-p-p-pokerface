(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst snd] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {"T" 10, "J" 11, "Q" 12, "K" 13, "A" 14} (str fst)))))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn pair? [hand]
  (if (= 2 (apply max (vals (frequencies (map rank hand)))))
    true
    false))


(defn three-of-a-kind? [hand]
  (if (= 3 (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn four-of-a-kind? [hand]
  (if (= 4 (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn flush? [hand]
  (if (= 5 (apply max (vals (frequencies (map suit hand)))))
    true
    false))

(defn full-house? [hand]
  (if (= '(3 2) (vals (frequencies (map rank hand))))
    true
    false))

(defn two-pairs? [hand]
  (if (= '(2 2 1) (vals (frequencies (map rank hand))))
    true
    false))

(defn straight? [hand]
  (let [sort-hand (sort (map rank hand))]
    (let [suora?  (range (apply min sort-hand) (+ 5 (apply min sort-hand)))]
      (if (= sort-hand suora?)
        true
        (let [low-ace-hand (sort (replace {14 1} sort-hand))]
          (if (= low-ace-hand '(1 2 3 4 5))
            true
            false))))))
  

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)


