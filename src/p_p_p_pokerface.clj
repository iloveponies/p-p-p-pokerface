(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (let [conversion {\T 10 \J 11 \Q 12 \K 13 \A 14}]
      (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (get conversion rank)))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map (fn [card] (rank card)) hand)]
    (not (empty? (filter (fn [x] (= 2 x)) (vals (frequencies ranks)))))))

(defn three-of-a-kind? [hand]
  (let [ranks (map (fn [card] (rank card)) hand)]
    (not (empty? (filter (fn [x] (= 3 x)) (vals (frequencies ranks)))))))

(defn four-of-a-kind? [hand]
  (let [ranks (map (fn [card] (rank card)) hand)]
    (not (empty? (filter (fn [x] (= 4 x)) (vals (frequencies ranks)))))))

(defn flush? [hand]
  (let [suits (map (fn [card] (suit card)) hand)]
    (let [uniq-suits (set suits)]
      (= 1 (count uniq-suits)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map (fn [card] (rank card)) hand)]
    (or (= 2 (count (filter (fn [x] (= 2 x)) (vals (frequencies ranks)))))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ordered-ranks (sort (map (fn [card] (rank card)) hand))]
    (let [ordered-ranks-ace-one (sort (replace {14 1} ordered-ranks))]
      (let [lowest (apply min ordered-ranks)]
        (let [lowest-ace-one (apply min ordered-ranks-ace-one)]
          (or (= ordered-ranks (range lowest (+ 5 lowest)))
              (= ordered-ranks-ace-one (range 1 6))))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [hands-types (mapv (fn [check] ((first check) hand)) checkers)]
    (apply max (filter hands-types (map (fn [check] (second check)) checkers)))))
