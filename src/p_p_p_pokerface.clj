(ns p-p-p-pokerface)

(def face-card-values {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank-char _] card]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (face-card-values rank-char))))

(defn suit [card]
  (let [[_ suit-char] card]
    (str suit-char)))

(defn max-with-same-rank [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn max-with-same-suit [hand]
  (apply max (vals (frequencies (map suit hand)))))

(defn high-card? [hand] true)

(defn pair? [hand]
  (= (max-with-same-rank hand) 2))

(defn three-of-a-kind? [hand]
  (= (max-with-same-rank hand) 3))

(defn four-of-a-kind? [hand]
  (= (max-with-same-rank hand) 4))

(defn flush? [hand]
  (= (max-with-same-suit hand) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (sort (vals (frequencies (map rank hand)))) [1 2 2])))

(defn straight? [hand]
  (let [ranks (map rank hand)
        test-straight (fn [ranks] (let [sorted (sort ranks)
                                        low (first sorted)]
                                    (= sorted (range low (+ low 5)))))]
    (or (test-straight ranks) (test-straight (replace {14 1} ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5] [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        is-match? (fn [check] ((first check) hand))]
    (apply max (map second (filter is-match? checkers)))
      ))
      ;(filter (fn [check] ((first check) hand)) checkers)
