(ns p-p-p-pokerface)

(defn rank [card]
  ;; kortin arvo
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  ;; kortin maa
   (let [[_ suit] card]
  (str suit))
  )

(defn high-card? [hand]
  true)

(defn pair? [hand]
  ;; tarkistaa onko kädessä yhtään paria
 (if (empty? (filter (fn [x] (= x 2)) (vals(frequencies(map rank hand)))))
   false
   true
   ))

(defn three-of-a-kind? [hand]
    ;; tarkistaa onko kädessä kolmea samaa
 (if (empty? (filter (fn [x] (= x 3)) (vals(frequencies(map rank hand)))))
   false
   true
   ))

(defn four-of-a-kind? [hand]
      ;; tarkistaa onko kädessä neljää samaa
 (if (empty? (filter (fn [x] (= x 4)) (vals(frequencies(map rank hand)))))
   false
   true
   ))

(defn flush? [hand]
  ;; tarkistaa onko väri
   (= 5 (first(vals(frequencies (map suit hand))))))

(defn full-house? [hand]
  ;; tarkistaa onko täyskäsi
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  ;; tarkistaa onko kaksi paria
  (= 2 (count (filter (fn [x] (= x 2)) (vals(frequencies(map rank hand)))))))

(defn straight? [hand]
  ;; tarkistaa onko suora
  (let [ace
(if (= (sort (map rank hand )) [2 3 4 5 14])
  (sort (replace {14 1} (map rank hand)))
  (sort (map rank hand )))]
(= (range (apply min ace) (+ 1 (apply max ace))) ace)))

(defn straight-flush? [hand]
  ;; tarkistaa onko värisuora
  (= true (straight? hand) (flush? hand))
  )

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
