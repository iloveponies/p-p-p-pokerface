  (ns p-p-p-pokerface)

(defn rank [card]
   (let [kirjainarvot {\T 10, \J 11, \Q 12, \K 13, \A 14}
         [eka ] card]

      (if (Character/isDigit eka)
        (Integer/valueOf (str eka))
        (get kirjainarvot eka))))


(defn suit [card]
  (let [[_ toka] card]
     (str toka)))

(defn pair? [hand]
  (let [esiintymiset (frequencies (map rank hand))]
     (<
       (count (vals esiintymiset))
        5)))


(defn three-of-a-kind? [hand]
   (let [esiintymiset (frequencies (map rank hand))
         esiintymismaarat (vals esiintymiset)]
      (>=
        (apply max esiintymismaarat)
        3)))

(defn four-of-a-kind? [hand]
  (let [esiintymiset (frequencies (map rank hand))
        esiintymismaarat (vals esiintymiset)
        maksimi (apply max esiintymismaarat)]
     (>=
       maksimi
       4)))

(defn flush? [hand]
  (let [esiintymiset (frequencies (map suit hand))
        esiintymismaarat (vals esiintymiset)
        maksimi (apply max esiintymismaarat)]
    (= 5 maksimi)))

(defn full-house? [hand]
  (let [esiintymiset (frequencies (map rank hand))
        esiintymismaarat (vals esiintymiset)
        minimi (apply min esiintymismaarat)
        maksimi (apply max esiintymismaarat)]
     (and
       (= 2 minimi)
       (= 3 maksimi))))

(defn two-pairs? [hand]
  (let [esiintymiset (frequencies (map rank hand))
        esiintymiskerrat (vals esiintymiset)
        jarjestetty (sort esiintymiskerrat)
        ]

    (or (= jarjestetty [1 4]) (= jarjestetty [1 2 2]))))


(defn straight? [hand]
  (let [arvot (map rank hand)
        jarjestetyt-arvot (sort arvot)
        minimi (apply min arvot)
        maksimi (apply max arvot)]
    (if (and (= minimi 2) (= maksimi 14))
      (= [2 3 4 5 14] jarjestetyt-arvot)
      (= (range minimi (+ minimi 5)) jarjestetyt-arvot))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

;apufunktio arvon laskemiselle
(defn high-card? [hand]
  true)

 (defn value [hand]
   (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}

     kaden-ominaisuudet (filter (fn [x] ((first x) hand)) checkers)
     kaden-arvot (map second kaden-ominaisuudet)]

     (apply max kaden-arvot)))
