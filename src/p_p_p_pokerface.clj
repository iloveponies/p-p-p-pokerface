(ns p-p-p-pokerface)

(defn rank [card]
  (let [v (first card)]
    (if (Character/isDigit v)
      (Integer/valueOf (str v))
      (cond (= \T v) 10
            (= \J v) 11
            (= \Q v) 12
            (= \K v) 13
            (= \A v) 14
            true (throw (Exception. (str "the goat enters")))))))
                                                          ; ^^^^ f-yeah.

(defn suit [card]
  (str (last card)))

(defn pair? [hand]
  (not (= nil (some (fn [x] (= x 2)) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (= nil (some (fn [x] (= x 3)) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (not (= nil (some (fn [x] (= x 4)) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  ; lulz
  (or (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [testfn (fn [foot] 
                (let [fst (first foot)
                      lst (last foot)]
                  (= (range fst (+ 1 lst)) foot)))
        sortedHand (sort (map rank hand))]
    (if (testfn sortedHand)
      true
      (if (= 14 (last sortedHand))
        (testfn (sort (assoc (vec sortedHand) 4 1)))
        false))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]
        sincere (map #(%1 hand) checkers)
        haha (reduce (fn [res item] (let [index (first res)
                                          resv (second res)]
                                      (if (= true item)
                                        (list (+ 1 index) index)
                                        (list (+ 1 index) resv))))
                     (list 0 0) sincere)]
    ; because we can
    (second haha)))

;    (map #(%1 hand) checkers)))
