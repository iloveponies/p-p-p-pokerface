(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card rnk-map {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (Integer/valueOf (if (Character/isDigit rnk)
                       (Integer/valueOf (str rnk))
                       (get rnk-map rnk)))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        cnts  (frequencies ranks)
        max-cnt (apply max (vals cnts))]
    (>= max-cnt 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        cnts  (frequencies ranks)
        max-cnt (apply max (vals cnts))]
    (>= max-cnt 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        cnts  (frequencies ranks)
        max-cnt (apply max (vals cnts))]
    (== max-cnt 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        cnts  (frequencies suits)
        max-cnt (apply max (vals cnts))]
    (== max-cnt 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        cnts  (sort (vals (frequencies ranks)))]
    (and (== (first cnts) 2) (== (last cnts) 3))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        cnts (sort (vals (frequencies ranks)))]
    (or (= cnts (seq [1 2 2])) (= cnts (seq [1 4])))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        h1    (sort ranks)
        h2    (sort (replace {14 1} ranks))
        c1    (first h1)
        c2    (first h2)
        s1    (range c1 (+ c1 5))
        s2    (range c2 (+ c2 5))]
    (or (= h1 (seq s1)) (= h2 (seq s2)))))

(defn straight-flush? [hand]
 (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        matches (filter (fn [x] ((first x) hand)) checkers)
        values  (map last matches)
        highest (apply max values)]
    highest))
