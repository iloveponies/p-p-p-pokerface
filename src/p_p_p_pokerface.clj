(ns p-p-p-pokerface)

(defn rank [card]
  (let [lut {\T 10
             \J 11
             \Q 12
             \K 13
             \A 14}
        [r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get lut r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn n-of-a-kind [hand n]
    (let [freqs (vals (frequencies (map rank hand)))]
    (== n (apply max freqs))))

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (let [freqs (vals (frequencies (map suit hand)))]
    (== 5 (apply max freqs))))

(defn full-house? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (= [2 3] (sort freqs))))

(defn two-pairs? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (or (= [1 2 2] freqs)
        (= [1 4] freqs))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        alt-ranks (replace {14 1} ranks)]
    (or (= (range (apply min ranks) (+ 1 (apply max ranks))) (sort ranks))
        (= (range (apply min alt-ranks) (+ 1 (apply max alt-ranks))) (sort alt-ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matched (filter (fn [checker] ((first checker) hand)) checkers)
        values (map second matched)]
    (apply max values)))
