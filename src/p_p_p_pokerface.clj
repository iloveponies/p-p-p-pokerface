(ns p-p-p-pokerface)

(defn rank [card]
  (let [substitutes {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [r _] card]
    (cond
      (Character/isDigit r) (Integer/valueOf (str r))
      :else (get substitutes r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn ranks [cards]
  (map rank cards))

(defn suits [cards]
  (map suit cards))

(defn freq-vals [hand type-func]
  (vals (frequencies (type-func hand))))

(defn contains-freq? [hand type-func freq]
  (contains?
    (set (freq-vals hand type-func)) freq))

(defn pair? [hand]
  (contains-freq? hand ranks 2))

(defn three-of-a-kind? [hand]
  (contains-freq? hand ranks 3))

(defn four-of-a-kind? [hand]
  (contains-freq? hand ranks 4))

(defn flush? [hand]
  (contains-freq? hand suits 5))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= 2 (count (filter (fn [v] (= v 2)) (freq-vals hand ranks)))))

(defn straight? [hand]
  (let [straight-help
        (fn [hand-ranks]
          (let [sorted-ranks (sort hand-ranks)
                hand-min (first sorted-ranks)
                straight (range hand-min (+ hand-min 5))]
            (= sorted-ranks straight)))]
    (or
      (straight-help (ranks hand))
      (straight-help (replace {14 1} (ranks hand))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check (fn [checker] ((first checker) hand))
        checked (filter check checkers)
        points (map second checked)]
    (apply max points)))
