(ns p-p-p-pokerface)

; Harjoitus 2
(defn rank
        [card]
        (let [[fst _] card]
                fst
                (if (Character/isDigit fst)
                        (Integer/valueOf (str fst))
                        (cond
                         (= fst \T)
                                10
                         (= fst \J)
                                11
                         (= fst \Q)
                                12
                         (= fst \K)
                                13
                         (= fst \A)
                                14
                        )
                )
        )
)

; Harjoitus 1
(defn suit
        [card]
        (let [[_ snd] card]
                snd
                (str snd)
        )
)

; Harjoitus 3
(defn pair?
        [hand]
        (let [kasi (map rank hand)]
          (let [taajuudet (vals (frequencies kasi))]
                (contains? (set taajuudet) 2)))
)


; Harjoitus 4
(defn three-of-a-kind?
        [hand]
        (let [kasi (map rank hand)]
          (let [taajuudet (vals (frequencies kasi))]
                (contains? (set taajuudet) 3)))
)

; Harjoitus 5
(defn four-of-a-kind?
        [hand]
        (let [kasi (map rank hand)]
          (let [taajuudet (vals (frequencies kasi))]
                (contains? (set taajuudet) 4)))
)

; Harjoitus 6
; meni näemmä samalla kaavalla kuin edelliset
(defn flush?
        [hand]
        (let [kasi (map suit hand)]
          (let [taajuudet (vals (frequencies kasi))]
                (contains? (set taajuudet) 5)))
)

; Harjoitus 7
; Tuntui simppeliltä ratkaisulta ja meni läpi
; Mutta onko kaikki kohdallaan?
(defn full-house?
        [hand]
        (and (pair? hand) (three-of-a-kind? hand))
)

; Harjoitus 8
(defn two-pairs?
        [hand]
        (if     (or

                (four-of-a-kind? hand)

                (let [kasi (map rank hand)]
                 (let [taajuudet (vals (frequencies kasi))]
                  (== (count (filter (fn [taaj] (== 2 taaj)) taajuudet)) 2))
                 )

                )
                )
                true
                false
                )
)

; Harjoitus 9
(defn straight?
        [hand]
        (let [kasi14 (map rank hand)
                kasi1 (replace {14,1} (map rank hand))]
        (if     (or

                (= (sort kasi14) (range (first (sort kasi14)) (+ 1 (last (sort kasi14)))))

                (= (sort kasi1) (range (first (sort kasi1)) (+ 1 (last (sort kasi1)))))

                )

                true
                false
        )
        )
)

; Harjoitus 10
(defn straight-flush?
        [hand]
        (if (and (straight? hand) (flush? hand))
                true
                false
        )
)

; Tehtävässä annettu funktio
; Tosin tehtävä menee noilla yo. funktioilla ihan hyvin
(defn high-card? [hand]
  true) ; All hands have a high card.

; Harjoitus 11
(defn value
        [hand]
        (cond
                (straight-flush? hand) 8
                (four-of-a-kind? hand) 7
                (full-house? hand) 6
                (flush? hand) 5
                (straight? hand) 4
                (three-of-a-kind? hand) 3
                (two-pairs? hand) 2
                (pair? hand) 1
                :else 0
        )
)
