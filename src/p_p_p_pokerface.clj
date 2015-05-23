(ns p-p-p-pokerface)
(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[fs _] card]
    (if (Character/isDigit fs)
      (Integer/valueOf (str fs))
      (Integer/valueOf (get replacements fs)))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn pair? [hand]
  (let [frekvenssit (frequencies (map rank hand))]
    (if (contains? (set (vals frekvenssit)) 2)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [frekvenssit (frequencies (map rank hand))]
    (if (contains? (set (vals frekvenssit)) 3)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [frekvenssit
        (frequencies
         (map rank hand))]
    (if (contains?
         (set
          (vals frekvenssit))
           4)
      true
      false)))

(defn flush? [hand]
  (let [maat
        (frequencies
         (map suit hand))]
    (if (contains?
         (set
          (vals maat))
           5)
      true
      false)))

(defn full-house? [hand]
  (let [jarjestettu
        (sort
         (vals
          (frequencies
            (map rank hand))))]
    (if (= [2 3] jarjestettu)
     true
     false)))

(defn two-pairs? [hand]
  (let [jarjestetty
        (sort
         (vals
          (frequencies
           (map rank hand))))]
    (if
      (or (= [1 2 2] jarjestetty)
          (= [1 4] jarjestetty))
    true
    false)))


(defn straight? [hand]
  (let [arvot_suuri
         (sort (map rank hand))
        arvot_pieni
         (sort (replace {14 1} arvot_suuri))]
    (if
      (or
        (= (range (first arvot_suuri)
                  (+ 1 (last arvot_suuri)))
            arvot_suuri)
        (= (range (first arvot_pieni)
                  (+ 1(last arvot_pieni)))
            arvot_pieni))
    true
    false)))

(defn straight-flush? [hand]
  (if
    (and
     (straight? hand)
     (flush? hand))
    true
    false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        values (filter
                (fn [x]
                  ((first x) hand)) checkers)]
    (apply max
           (map second values))))


