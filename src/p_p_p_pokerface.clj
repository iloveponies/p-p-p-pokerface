(ns p-p-p-pokerface)



;;(def value-of-images {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst snd] card
        value-of-images {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get value-of-images fst))))


(defn suit [card]
  (let [ [fst snd ] card]
    (str snd)))


 
(defn pair? [hand]
  (let [arvot (map rank hand)
        tiheydet (vals (frequencies arvot))
        suurin (apply max tiheydet)]
    (if (= suurin 2)
      true
      false)))


(defn three-of-a-kind? [hand]
  (let [arvot (map rank hand)
        tiheydet (vals (frequencies arvot))
        suurin (apply max tiheydet)]
    (if (= suurin 3)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [arvot (map rank hand)
        tiheydet (vals (frequencies arvot))
        suurin (apply max tiheydet)]
    (if (= suurin 4)
      true
      false)))

(defn flush? [hand]
  (let [tyypit (map suit hand)]
    (apply = tyypit)))

(defn full-house? [hand]
  (let [arvot (map rank hand)
        tiheydet (vals (frequencies arvot))
        suurin (apply max tiheydet)
        pienin (apply min tiheydet)]
    (and
         (= suurin 3)
         (= pienin 2))))

(defn two-pairs? [hand]
  (let [arvot (map rank hand)
        tiheydet (vals (frequencies arvot))
        parit (filter (fn [x] (= 2 x)) tiheydet)]
    (= (count parit) 2)))



(defn straight? [hand]
  (let [arvot (map rank hand)
        pienin (apply min arvot)
        suurin (apply max arvot)
        suoristettu (sort arvot)
        aito-mono (apply < suoristettu)]
    (cond
      (and aito-mono (= 4 (- suurin pienin))) true
      (= suoristettu [2 3 4 5 14]) true
      :else false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

;(defn value [hand]
;  (let [arvot #{[high-card? 0] [pair? 1]
;                [two-pairs? 2] [three-of-a-kind? 3]
;                [straight? 4] [flush? 5]
;                [full-house? 6] [four-of-a-kind? 7]
;                [straight-flush? 8]}]
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
