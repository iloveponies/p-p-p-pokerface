(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {
                      \T 10
                      \J 11
                      \Q 12
                      \K 13
                      \A 14
                      }]
    (let [[rank, _] card]
      (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (replacements rank)))))

(defn suit [card]
  (let [[_, suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (==
      (apply max (vals (frequencies ranks)))
      2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (==
      (apply max (vals (frequencies ranks)))
      3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (==
    (apply max (vals (frequencies ranks)))
    4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (==
      (first (vals (frequencies suits)))
      5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (=
      (sort (vals (frequencies ranks)))
      (range 2 4))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (or
      (=
        (sort (vals (frequencies ranks)))
        '(1 2 2))
      (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (let [start (apply min ranks)]
      (let [has-ace (contains? (set ranks) 14)]
        (cond
          has-ace (or
                      (= (sort ranks) (range 10 15))
                      (= (sort (replace {14 1} ranks)) (range 1 6)))
          :else (=
                  (sort ranks)
                  (range start (+ start 5))))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

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
