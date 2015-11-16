(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        bigCards {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get bigCards r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn has-freq? [freq hits hand]
  (== hits
      (count
        (filter
          (fn [n] (== n freq))
          (vals (frequencies (map rank hand)))))))

(defn pair? [hand]
  (has-freq? 2 1 hand))

(defn three-of-a-kind? [hand]
  (has-freq? 3 1 hand))

(defn four-of-a-kind? [hand]
  (has-freq? 4 1 hand))

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (and (has-freq? 3 1 hand) (has-freq? 2 1 hand)))

(defn two-pairs? [hand]
  (or (has-freq? 4 1 hand) (has-freq? 2 2 hand)))

(defn straight? [hand]
  (let [ranks (sort < (map rank hand))
        smallest (apply min ranks)]
    (or
      (= ranks (range smallest (+ smallest 5)))
      (= (sort < (replace {14 1} ranks)) (range 1 6)))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn value [hand]
  nil)
