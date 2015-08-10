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
  (is-same? hand 2) (is-same? hand 3))

(defn two-pairs? [hand]
(is-same? hand 2) (is-same? hand 2))

(defn straight? [hand]
  (let [numerot(for [x hand]
  (read-string(str (rank x))))
        parit(frequencies numerot)
        p(keys parit)
        maks(apply max p)
         m2(Integer/valueOf (str maks))
        minimi(apply min p)
        m1 (Integer/valueOf (str minimi))
        v1(contains? parit (+ m1 1))
        v2(contains? parit (+ m1 2))
        v3(contains? parit (- m2 1))
        ]
 (boolean v2)(boolean v1) (boolean v3)
    ))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
