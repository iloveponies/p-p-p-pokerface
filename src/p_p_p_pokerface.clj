(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank suit] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (get replacements rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn n-same-cards-by [typ cards n]
  (let [values (map typ cards)
        freqs (frequencies values)
        same? (fn [key] (== n (get freqs key)))]
    (filter same? (keys freqs))))

(defn n-same-ranks [cards n]
  (n-same-cards-by rank cards n))

(defn n-same-suits [cards n]
  (n-same-cards-by suit cards n))

(defn pair? [hand]
  (== 1 (count (n-same-ranks hand 2))))

(defn three-of-a-kind? [hand]
  (== 1 (count (n-same-ranks hand 3))))

(defn four-of-a-kind? [hand]
  (== 1 (count (n-same-ranks hand 4))))

(defn flush? [hand]
  (== 1 (count (n-same-suits hand 5))))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (== 2 (count (n-same-ranks hand 2))))

(defn straight [eka]
  (range eka (+ eka 5)))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        eka (first sorted)
        vika (last sorted)
        suora (straight eka)]
    (or
      (= sorted suora)
      (= (straight 1) (sort (replace {14 1} sorted))))))

(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))

;; Hand                  Value
;; ===========================
;; High card (nothing)     0
;; Pair                    1
;; Two pairs               2
;; Three of a kind         3
;; Straight                4
;; Flush                   5
;; Full house              6
;; Four of a kind          7
;; Straight flush          8

(defn high-card? [hand]
  true)

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

