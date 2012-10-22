(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        map {\T 10, \J 11, \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r) (Integer/valueOf (str r)) (get map r))))

(defn at-least [n hand]
  (let [ranks (map rank hand)]
    (boolean (<= n (apply max (vals (frequencies ranks)))))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (at-least 2 hand))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        ge (fn [x] (>= x 2))
        filtered (filter ge (vals (frequencies ranks)))]
    (boolean (or (== (count filtered) 2) (== 4 (first filtered))))))

(defn three-of-a-kind? [hand]
  (at-least 3 hand))

(defn four-of-a-kind? [hand]
  (at-least 4 hand))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        rep1 (sort (replace {1 14} ranks))
        rep2 (sort (replace {14 1} ranks))]
    (boolean (or (= ranks (range (get ranks 0) ((get ranks 0) + 6)))
             (= rep1 (range (get rep1 0) ((get rep1 0) + 6)))
             (= rep2 (range (get rep2 0) ((get rep2 0) + 6)))))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (boolean (== 1 (count (set suits))))))

(defn full-house? [hand]
  (let [kinds (vals (frequencies (map rank hand)))]
    (boolean (== 2 (count kinds)))))

(defn straight-flush? [hand]
  (boolean (and (straight? hand) (flush? hand))))

(defn value [hand]
  nil)