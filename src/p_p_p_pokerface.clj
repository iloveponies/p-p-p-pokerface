(ns p-p-p-pokerface)

(def big-cards
  {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rk _] card]
    (if (Character/isDigit rk)
      (Integer/valueOf (str rk))
      (big-cards rk))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn sorted-freqs [hand]
  (let [ranks (map rank hand)]
    (sort (vals (frequencies ranks)))))

(defn pair? [hand]
  (let [freqs (sorted-freqs hand)]
    (= freqs [1 1 1 2])))

(defn three-of-a-kind? [hand]
  (let [freqs (sorted-freqs hand)]
    (= freqs [1 1 3])))

(defn four-of-a-kind? [hand]
  (let [freqs (sorted-freqs hand)]
    (= freqs [1 4])))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [freqs (sorted-freqs hand)]
    (= freqs [2 3])))

(defn two-pairs? [hand]
  (let [freqs (sorted-freqs hand)]
    (or (= freqs [1 2 2])
        (= freqs [1 4]))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        str-fn (fn [rks]
                 (let [fst (apply min rks)]
                   (= (sort rks)
                      (range fst (+ fst 5)))))]
    (or (str-fn ranks)
        (str-fn (replace {14 1} ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        chk-fn (fn [chk] ((first chk) hand))
        values (map second (filter chk-fn checkers))]
    (apply max values)))
