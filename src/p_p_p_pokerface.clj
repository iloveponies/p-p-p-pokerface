(ns p-p-p-pokerface)



(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst)
  )))

(defn rankhand [hand]
  (map rank hand)
    )

(defn suit [card]
  (let [[_ snd] card]
    (str snd)
  ))
(defn sorted-keys [hand]
  (sort
   (rankhand hand))
       )

(defn sorted-keys-frequensies [hand]
(let [[[f1 ] [f2 _] [f3 _] [f4 _] [f5 _]]  hand]
  (sort
   (vals
      (frequencies
        [f1 f2 f3 f4 f5]
       )))))

(defn maksimi [hand]
     (apply max
       (sorted-keys-frequensies hand)
         ))

(defn minimi [hand]
   (apply min
    (sorted-keys-frequensies hand)
       ))

(defn pair? [hand]
  (if
      (and
         (= (maksimi hand) 2)
         (= (minimi hand) 1))
      true
      false
      ))

(defn three-of-a-kind? [hand]
  (if
      (and
         (= (maksimi hand) 3)
         (= (minimi hand) 1))
      true
      false
      ))

(defn full-house? [hand]
  (if
      (and
         (= (maksimi hand) 3)
         (= (minimi hand) 2))
      true
      false
      ))

(defn four-of-a-kind? [hand]

   (if
      (= (maksimi hand) 4)
      true
      false
      ))


(defn flush? [hand]
  (let [[[_ s1] [_ s2] [_ s3] [_ s4] [_ s5]]  hand]
  ;  (if
  ;    (= 5
  ; (apply max
    (if
      (= 5(
   apply max(
      vals(frequencies
          [s1 s2 s3 s4 s5]
           ))))
      true
      false)))
    ;  true
     ; false
     ; )))

(defn two-pairs? [hand]
    (or
     (= [1 2 2]  (sorted-keys-frequensies hand))
      (= [1 4] (sorted-keys-frequensies hand
                                        ))))

 (defn has-ace [hand]
    (if
     (= 14 ( apply max hand))
      true
      false
      ))

 (defn replace-ace [hand]

      (replace {14 1} hand)

   )

 (defn sorted-ace-replaced-equals-straight? [hand]

         (=  (sort(replace-ace(sorted-keys hand)))[1 2 3 4 5])

       )


(defn straight? [hand]
  (let [range-match? (fn [hand](= (sorted-keys hand) (range (first(sorted-keys hand)) (+ (first(sorted-keys hand)) 5))))]
 (if
    (or
    (range-match? hand)
    (sorted-ace-replaced-equals-straight? hand)
     )
   true
   false
   )

  ))

(defn straight-flush? [hand]
  (if
    (and (straight? hand) (flush? hand))
    true
    false
  ))
(defn high-card? [hand]
  true) ; All hands



(defn value [hand]



   (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand ) 1

    :else         0))


