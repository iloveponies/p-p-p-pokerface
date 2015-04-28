(ns p-p-p-pokerface)

(def the-cards-i-love
  {\T 10,
   \J 11,
   \Q 12,
   \K 13,
   \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r) (Integer/valueOf (str r)) (the-cards-i-love r))))

(defn suit [card]
  (str (get card 1)))

(defn hand-has? [hand freq where min]
  (let [n (count (filter (fn [x] (== x freq)) (vals(frequencies (map where hand)))))]
  (> n min)))

(defn pair? [hand]
  (hand-has? hand 2 rank 0))

(defn three-of-a-kind? [hand]
  (hand-has? hand 3 rank 0))

(defn four-of-a-kind? [hand]
  (hand-has? hand 4 rank 0))

(defn flush? [hand]
  (hand-has? hand 5 suit 0))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (hand-has? hand 2 rank 1))

(defn straight? [hand]
  (let [high-ace (sort (map rank hand))
        low-ace (sort (replace {14 1} high-ace))
        ordered-seq? (fn [seq] (= (range (apply min seq) (+ 5 (apply min seq))) seq))]
    (or (ordered-seq? high-ace) (ordered-seq? low-ace))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [check] (if ((first check) hand) (second check) 0)) checkers))))


