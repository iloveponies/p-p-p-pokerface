(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        replacements {\A 14, \T 10, \J 11, \Q 12, \K 13}]
    (Integer/valueOf (if (Character/isDigit fst)
                       (str fst)
                       (replacements fst)))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        rank-freq (frequencies ranks)
        pairs (filter (fn [x] (== x 2)) (vals rank-freq))]
    (not (empty? pairs))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-freq (frequencies ranks)
        threes (filter (fn [x] (== x 3)) (vals rank-freq))]
    (not (empty? threes))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-freq (frequencies ranks)
        fours (filter (fn [x] (== x 4)) (vals rank-freq))]
    (not (empty? fours))))

(defn flush? [hand]
  (let [suits (map suit hand)
        suits-freq (frequencies suits)]
    (== (first (vals suits-freq)) (count hand))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        rank-freq (frequencies ranks)
        pairs (filter (fn [x] (== x 2)) (vals rank-freq))
        trips (filter (fn [x] (== x 3)) (vals rank-freq))]
    (and (== (count rank-freq) 2)
         (== (count pairs) (count trips) 1))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-freq (frequencies ranks)
        pairs (filter (fn [x] (== x 2)) (vals rank-freq))]
    (== 2 (count pairs))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        rank-min (apply min ranks)
        rank-max (apply max ranks)
        low-ace (and (== rank-min 2)
                     (== rank-max 14))
        lowest-range (if low-ace 1
                         rank-min)
        highest-range (if low-ace 5
                          rank-max)
        test-range (range lowest-range (+ 1 highest-range))
        new-ranks (if low-ace
                    (replace {14 1} ranks)
                    ranks)]
    (= test-range (sort < new-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        scores (map (fn [c] (if ((first c) hand)
                              (second c)
                              0)) checkers)]
    (apply max scores)))
        
