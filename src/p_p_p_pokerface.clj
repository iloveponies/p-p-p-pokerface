(ns p-p-p-pokerface)

(defn rank [card]
  (let [[card-rank _] card]
    (def replacements {\T 10
                       \J 11
                       \Q 12
                       \K 13
                       \A 14})
    (if (Character/isDigit card-rank)
      (Integer/valueOf (str card-rank))
      (replacements card-rank))))

(defn suit [card]
  (let [[_ card-suit] card]
    (str card-suit)))

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (or (= [1 4] freqs) (= [1 2 2] freqs))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        replaced-ranks (sort (replace {14 1} (map rank hand)))]
    ;; (println ranks)
    ;; (println replaced-ranks)
    ;; (println (range (first ranks) (+ 1 (last ranks))))
    (or
     (= (range (first ranks) (+ 1 (last ranks))) ranks)
     (= (range (first replaced-ranks) (+ 1 (last replaced-ranks))) replaced-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [checker] ((first checker) hand)) checkers)))
    ))
