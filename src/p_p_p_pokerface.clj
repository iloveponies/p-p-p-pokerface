(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst snd] card
        arvo(Character/isDigit fst)
        returnValue(if (= arvo true)
          (Integer/valueOf (str fst))
          (replacements fst))
        ] returnValue
  ))

(defn suit [card]
  (let [[fst snd] card]
  (str snd)))

(defn is-same? [hand maksimi]
  (let [numerot(for [x hand]
  (read-string(str (rank x))))
        parit(frequencies numerot)
        p(vals parit)
        maks(apply max p)
        returnValue(if(== maks maksimi)
          true
          false)
        ]
    returnValue))

(defn pair? [hand]
  (is-same? hand 2))

(defn three-of-a-kind? [hand]
  (is-same? hand 3))

(defn four-of-a-kind? [hand]
  (is-same? hand 4))

(defn flush? [hand]
  (let [maa(for [x hand]
   (read-string(str (suit x))))
      eka (first maa)
        te(distinct maa)
        summa(count te)]
    (if (= summa 1)
      true
      false))
    )

(defn full-house? [hand]
  (let [numerot(for [x hand]
  (read-string(str (rank x))))
        parit(frequencies numerot)
        p(vals parit)
        maks(apply max p)

        minimi(apply min p)
        returnValue(if(and (== maks 3) (== minimi 2))
         true
        false)
        ]
    returnValue)
)


(defn two-pairs? [hand]
  (let [numerot(for [x hand]
  (read-string(str (rank x))))
        parit(frequencies numerot)
        p(vals parit)
        maks(apply max p)
        summa (count p)
        minimi(apply min p)
        returnValue(if(and(and (== maks 2) (== minimi 1) (== summa 3)))
         true
        false)
        ]
    returnValue))


(defn straight? [hand]
  (let [numerot(for [x hand]
  (read-string(str (rank x))))
        parit(frequencies numerot)
        p(keys parit)
        maks(apply max p)
         m2(Integer/valueOf (str maks))
        minimi(apply min p)
        m1 (Integer/valueOf (str minimi))

        ace(if(= m1 2)
                  (contains? parit 14)
                  (contains? parit 1))

        summa(count p)
        viisi(if(= summa 5)
               true
               false)
        v1(contains? parit (+ m1 1))
        v2(contains? parit (+ m1 2))
        v3(contains? parit (- m2 1))
        v3(if(= ace true)
            true
            (contains? parit (- m2 1)))

          returnValue(if(and(and(and (= v1 true) (= v2 true) (= v3 true) (= viisi true))))
         true
        false)
        ]
returnValue
    ))

(defn straight-flush? [hand]
   (let [
  suora(straight? hand)
  vari (flush? hand)
         returnValue(if (and (= vari true) (= suora true))
         true
        false)]
returnValue)
  )

(defn value [hand]
  nil)
