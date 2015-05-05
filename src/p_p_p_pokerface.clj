(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        repl {\T 10
              \J 11
              \Q 12
              \K 13
              \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get repl r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [two? (fn [i] (= i 2))]
    (not (empty? (filter two? (vals (frequencies (map rank hand))))))))

(defn three-of-a-kind? [hand]
  (let [three? (fn [i] (= i 3))]
    (not (empty? (filter three? (vals (frequencies (map rank hand))))))))

(defn four-of-a-kind? [hand]
  (let [four? (fn [i] (= i 4))]
    (not (empty? (filter four? (vals (frequencies (map rank hand))))))))

(defn flush? [hand]
  (let [five? (fn [i] (= i 5))]
    (not (empty? (filter five? (vals (frequencies (map suit hand))))))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [two? (fn [i] (= i 2))]
    (= 2 (count (filter two? (vals (frequencies (map rank hand))))))))

(defn straight? [hand]
  (let [m {14 1}]
    (or (apply < (sort (map rank hand)))
        (apply < (sort (replace m (map rank hand)))))))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
