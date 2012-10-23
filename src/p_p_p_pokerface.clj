(ns p-p-p-pokerface)

(defn rank [[r _]]
  (let [repl {\T 10
              \J 11
              \Q 12
              \K 13
              \A 14}]
    (if (Character/isDigit r) (Integer/valueOf (str r)) (get repl r))))

(defn suit [[_ s]]
  (str s))

(defn amountChecker [fil hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)]
   (not (empty? (filter fil (vals freq))))))

(defn pair? [hand]
  (amountChecker (fn [x] (> x 1)) hand)
  )

(defn three-of-a-kind? [hand]
  (amountChecker (fn [x] (> x 2)) hand))

(defn four-of-a-kind? [hand]
  (amountChecker (fn [x] (> x 3)) hand))

(defn two-pairs? [hand]
 (let [ranks (map rank hand)
       freq (frequencies ranks)
       pairs (> (count (filter (fn [x] (> x 1)) (vals freq))) 1)]
   (or pairs (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks14 (map rank hand)
        ranks1 (replace {14 1} ranks14)
        maxR (fn [h] (apply max h))
        minR (fn [h] (apply min h))
        straight (fn [ranks] (= (sort ranks) (range (minR ranks) (+ (maxR ranks) 1))))]
    (or (straight ranks14) (straight ranks1))))

(defn flush? [hand]
  (let [suits (map suit hand)
        freq (frequencies suits)]
   (== (count freq) 1)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freq (sort (vals (frequencies ranks)))
        fh (seq [2 3])]
    (= freq fh)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0],  [pair? 1],
                   [two-pairs? 2],  [three-of-a-kind? 3],
                   [straight? 4],   [flush? 5],
                   [full-house? 6], [four-of-a-kind? 7],
                   [straight-flush? 8]}
        values (map (fn [[f, v]] (if (f hand) v 0)) checkers)]
   (apply max values)))