(ns p-p-p-pokerface)

(defn rank [card]
  (let [[f s] card
        charmap {\T 10, \J 11,
                 \Q 12, \K 13,
                 \A 14}]
    (if (Character/isDigit f)
      (Integer/valueOf (str f))
      (charmap f))))

(defn suit [card]
  (let [[f s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (== (apply max (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (== (apply max (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (== (apply max (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (= freqs [2 3])))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (or (= freqs [1 2 2])
        (= freqs [1 4]))))

(defn straight? [hand]
  (let [ranks1 (sort (map rank hand))
        low1 (first ranks1)
        ranks2 (sort (replace {14 1} ranks1))
        low2 (first ranks2)
        straight1 (range low1 (+ low1 5))
        straight2 (range low2 (+ low2 5))]
    (or (= ranks1 straight1) (= ranks2 straight2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        filtered (filter (fn [x] ((first x) hand)) checkers)
        values (map second filtered)]
    (apply max values)))
