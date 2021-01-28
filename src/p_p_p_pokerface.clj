(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank suit] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        same (apply max(vals (frequencies ranks)))]
    (< 1 same)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        same (apply max(vals (frequencies ranks)))]
    (< 2 same)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        same (apply max(vals (frequencies ranks)))]
    (< 3 same)))

(defn flush? [hand]
  (let [suits (map suit hand)
        same (apply max(vals (frequencies suits)))]
    (< 4 same)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        largest (apply max(vals (frequencies ranks)))
        smallest (apply min(vals (frequencies ranks)))]
    (and (< 2 largest) (< 1 smallest))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq (sort (vals (frequencies ranks)))]
    (or (and (= 2 (nth freq 1)) (= 2 (nth freq 2))) (== 4 (last freq)))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ace-hand ranks
        one-hand (sort (replace {14 1} ranks))]
    (or (= (range (first ace-hand) (+ (first ace-hand) 5)) ace-hand) (= (range 1 6) one-hand))))

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
        check (fn [checker]
                (let [[matcher value] checker]
                  (if (matcher hand)
                    value
                    0)))]
    (apply max (map check checkers))))
