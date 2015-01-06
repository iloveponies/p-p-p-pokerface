(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 4)))

(defn flush? [hand]
  (let [suits (set (map suit hand))]
    (= (count suits) 1)))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        pairs (filter (fn [num] (>= num 2)) freqs)]
    (or (= (count pairs) 2)
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        alt-ranks (replace {14 1} ranks)
        low (apply min ranks)
        alt-low (apply min alt-ranks)
        test (range low (+ low 5))
        alt-test (range alt-low (+ alt-low 5))]
    (or (= (sort ranks) test)
      (= (sort alt-ranks) alt-test))))

(defn straight-flush? [hand]
  (and (straight? hand)
      (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        app (fn [[fun val]] (if (fun hand) val))
        values (map app checkers)
        not-nil (fn [val] (not (nil? val)))
        non-nils (filter not-nil values)]
    (apply max non-nils)))
