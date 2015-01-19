(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        replacements {\T 10, \J 11, \Q 12, \K 13,\A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        has-pair (some #(= 2 %) (vals freq))]
    (not (= has-pair nil ))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        has-toak (some #(= 3 %) (vals freq))]
    (not (= has-toak nil))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        has-foak (some #(= 4 %) (vals freq))]
    (not (= has-foak nil))))

(defn flush? [hand]
  (let [suits (map suit hand)
        freq (frequencies suits)]
    (every? #(= 5 %) (vals freq))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        pairs (filter (fn [x] (= 2 (val x))) freq)]
    (or 
     (= 2 (count pairs))
     (not (= nil (some #(= 4 %) pairs))))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        keys (keys freq)
        keys-with-ace keys
        keys-with-one (replace {14 1} keys)
        range-4 (fn [x] (= 4 (- (apply max x) (apply min x))))]
    (and (= 5 (count keys))
         (or
          (and 
           (apply < (sort keys-with-one))
           (range-4 keys-with-one))             
          (and
           (apply < (sort keys-with-ace))
           (range-4 keys-with-ace))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                  [two-pairs? 2] [three-of-a-kind? 3]
                  [straight? 4] [flush? 5]
                  [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]}
        asdf (fn [x] (let [[f p] x] (if (f hand) p 0)))]
    (apply max (map asdf checkers))))

