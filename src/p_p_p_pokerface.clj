(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\A 14, \K 13 \Q 12, \J 11, \T 10})
  (let [[first _] card]
  (if (Character/isDigit first) (Integer/valueOf (str first)) (replacements first)))
)

(defn suit [card]
  (let [[_ snd] card]
  (if (= nil snd) (str _) (str snd)))
)

(defn pair? [hand]
  (let [ranks (map rank hand)]
      (contains? (set (vals (frequencies ranks))) 2)
  )
)

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
      (contains? (set (vals (frequencies ranks))) 3)
  )
)

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
      (contains? (set (vals (frequencies ranks))) 4)
  )
)

(defn flush? [hand]
  (let [suits (map suit hand)]
      (contains? (set (vals (frequencies suits))) 5)
  )
)

(defn full-house? [hand]
    (let [ranks (sort (map rank hand))]
      (and (contains? (set (vals (frequencies ranks))) 2) (contains? (set (vals (frequencies ranks))) 3))
  )
)

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        value-freq (vals (frequencies ranks))
        pair-count (count (filter (fn [f] (= 2 f)) value-freq))]
    (or
      (= 2 pair-count)
      (four-of-a-kind? hand)))
)

(defn straight? [hand]
  (let [ranks-a (sort (map rank hand))
        ranks-b (sort (replace {14 1} (map rank hand)))
        ranks-c (sort (replace {1 14} (map rank hand)))
       [a b c d e] ranks-a
       [aa bb cc dd ee ] ranks-b
       [aaa bbb ccc ddd eee ] ranks-c]
    (or
      (and (= e (+ 1 d)) (= d (+ 1 c)) (= c (+ 1 b)) (= b (+ 1 a)))
      (and (= ee (+ 1 dd)) (= dd (+ 1 cc)) (= cc (+ 1 bb)) (= bb (+ 1 aa)))
      (and (= eee (+ 1 ddd)) (= ddd (+ 1 ccc)) (= ccc (+ 1 bbb)) (= bbb (+ 1 aaa)))
    )
  )
)

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand))
)

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]

    (apply max (map second
           (filter (fn [checker]
           ((first checker) hand)) checkers)))
  )
)
