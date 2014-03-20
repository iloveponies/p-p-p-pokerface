(ns p-p-p-pokerface)

(defn rank [[r _]]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get replacements r))))

(defn suit [[_ s]]
  (str s))

(defn ranks [hand]
  (map rank hand))

(defn suits [hand]
  (map suit hand))

(defn rank-counts [hand]
  (vals (frequencies (ranks hand))))

(defn same-of-a-kind [hand kind]
  (let [of-kind (fn [x] (= x kind))]
    (not (empty? (filter of-kind (rank-counts hand))))))

(defn pair? [hand]
  (same-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (same-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (same-of-a-kind hand 4))

(defn flush? [hand]
  (apply = (suits hand)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= 2 (get (frequencies (rank-counts hand)) 2)))

(defn straight? [hand]
  (let [aces-as-fourteens (sort (ranks hand))
        aces-as-ones (sort (replace {14 1} (ranks hand)))
        straight (fn [rs] (range (first rs) (+ (first rs) 5)))
        func (fn [rs] (= rs (straight rs)))]
    (or (func aces-as-fourteens)
        (func aces-as-ones))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        second (fn [[_, y]] y)
        checker (fn [x] ((first x) hand))]
    (apply max (map second (filter checker checkers)))))
