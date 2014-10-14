(ns p-p-p-pokerface)

(defn in? [seq elem]
  (boolean (some #(= % elem) seq)))

(defn high-card? [hand]
  true)

(defn rank [ [r _] ]
  (def values {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (if (Character/isDigit r )
    (Integer/valueOf (str r))
    (values r)))

(defn suit [ [_ s] ]
  (str s))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== (apply max (vals (frequencies suits))) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= (range 2 4) (sort(vals (frequencies ranks))))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (= (seq [1 2 2]) (sort(vals (frequencies ranks))))))

(defn straight? [hand]
  (let [ranks   (sort(map rank hand))
        low-ace (sort(replace {14 1} ranks))
        card-range (fn [x] (range (apply min x) (+ (apply max x) 1)))]
    (if (in? ranks 2)
      (= low-ace (card-range low-ace))
      (= ranks (card-range ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]   [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hands    (filter #((first %) hand) checkers)]
    (apply max (map second hands))))




