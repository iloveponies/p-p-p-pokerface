(ns p-p-p-pokerface)

(defn rank [card]
  (let [lookup {\T 10 \J 11 \Q 12 \K 13 \A 14}
        rank->int (fn [x]
                    (if (Character/isDigit x) (Integer/valueOf (str x))))
        valid? (fn [x]
                 (cond (contains? lookup x) (get lookup x)
                       :else (rank->int x)))]
    (valid? (first card))))

(defn suit [card]
  (str (second card)))

(defn freqs [hand]
  (let [ranksh (map rank hand)
        freq (frequencies ranksh)]
    (sort (vals freq))))

(defn pair? [hand]
  (let [ranks (freqs hands)
        pairs (filter (fn [x] (>= x 2)) vals)]
    (not (empty? pairs))))

(defn three-of-a-kind? [hand]
  (let [ranks (freqs hands)
        pairs (filter (fn [x] (>= x 3)) vals)]
    (not (empty? pairs))))

(defn four-of-a-kind? [hand]
  (let [ranks (freqs hands)
        pairs (filter (fn [x] (>= x 4)) vals)]
    (not (empty? pairs))))

(defn flush? [hand]
  (let [suits (map suit hand)
        freq (frequencies suits)
        vals (vals freq)
        flush (filter (fn [x] (>= x 4)) vals)]
    (not (empty? flush))))

(defn full-house? [hand]
  (let [ranks (rank hand)]
    (= [2 3] valss)))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
