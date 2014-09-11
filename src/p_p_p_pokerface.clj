(ns p-p-p-pokerface)

(def ranks {\T 10 \J 11 \Q 12 \K 13 \A 14})
(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (ranks fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [rnks (map rank hand)
        freqs (frequencies rnks)
        vs (vals freqs)]
    (= 2 (apply max vs))))

(defn three-of-a-kind? [hand]
  (let [rnks (map rank hand)
        freqs (frequencies rnks)
        vs (vals freqs)]
    (= 3 (apply max vs)))  )

(defn four-of-a-kind? [hand]
  (let [rnks (map rank hand)
        freqs (frequencies rnks)
        vs (vals freqs)]
    (= 4 (apply max vs)))  )

(defn flush? [hand]
  (let [suts (map suit hand)
        freqs (frequencies suts)
        vs (vals freqs)]
    (= 5 (apply max vs))))

(defn full-house? [hand]
  (let [rnks (map rank hand)
        freqs (frequencies rnks)
        vs (vals freqs)]
    (= [2 3] (sort vs))))

(defn two-pairs? [hand]
  (let [rnks (map rank hand)
        freqs (frequencies rnks)
        vs (vals freqs)
        vss (frequencies vs)]
    (= 2 (vss 2))))

(defn straight? [hand]
  (let [rnks (map rank hand)
        mn (apply min rnks)
        mx (apply max rnks)]
    (if (and (= 14 mx) (= (range 10 (inc mx)) (sort rnks)))
      true
      (if (= (range mn (inc mx)) (sort rnks))
        true
        (and (= 2 mn)
             (= 14 mx)
             (= (range 1 6)
                (sort (replace {14 1} rnks))))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map #(second %) (filter (fn [[f v]] (f hand)) checkers)))))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
