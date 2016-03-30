(ns p-p-p-pokerface)

(def replacement {\T 10
                  \J 11
                  \Q 12
                  \K 13
                  \A 14})

;(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
;(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
;(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
;(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
;(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
;(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
;(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
;(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
;(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
;(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
;(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
;(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
;(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(defn rank [card]
  (let [[fst snd] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacement fst)
  )))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)
    ))

(defn pair? [hand]
  (contains?
    (set
      (vals
        (frequencies
          (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains?
    (set
      (vals
        (frequencies
          (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains?
    (set
      (vals
        (frequencies
          (map rank hand)))) 4))

(defn flush? [hand]
  (contains?
    (set
      (vals
        (frequencies
          (map suit hand)))) 5))

(defn full-house? [hand]
  (and
    (contains?
      (set
        (vals
          (frequencies
            (map rank hand)))) 3)
    (contains?
      (set
        (vals
          (frequencies
            (map rank hand)))) 2)))

(defn two-pairs? [hand]
  (contains?
    (set
      (vals
        (frequencies
         (vals
          (frequencies
           (map rank hand)))))) 2))

(defn straight? [hand]
  (or
   (= (sort
       (map rank hand))
      (range (first (sort (map rank hand)))
             (inc (first (reverse (sort (map rank hand)))))
         ))
    (= (sort
       (replace {14 1} (map rank hand)))
      (range (first (sort (replace {14 1} (map rank hand))))
             (inc (first (reverse (sort (replace {14 1} (map rank hand))))))
         ))))
;Supongo que me vas a cagar a pedos por este quilombo :$

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (= 5
       (first
         (vals
           (frequencies
             (map suit hand)))))))

(defn value [hand]
  nil)
