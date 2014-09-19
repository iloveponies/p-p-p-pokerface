(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (> (apply max (vals (frequencies ranks))) 1)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (> (apply max (vals (frequencies ranks))) 2)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (> (apply max (vals (frequencies ranks))) 3)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (> (apply max (vals (frequencies suits))) 4)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= (vals (frequencies ranks)) '(3 2))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (= (vals (frequencies ranks)) '(2 2 1))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand)) replaced (sort (replace {14 1} ranks))]
    (or
      (= ranks (range (first ranks) (+ (first ranks) 5)))
      (= replaced (range (first replaced) (+ (first replaced) 5))))))


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value-checker [checker hand]
  (if ((first checker) hand)
    (second checker)
    0))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map #(value-checker % hand) checkers))))
