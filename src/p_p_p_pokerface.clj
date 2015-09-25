(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn values [hand]
  (vals (frequencies (map (fn [card] (rank card)) hand))))

(defn suits [hand]
  (vals (frequencies (map (fn [card] (suit card)) hand))))

(defn pair? [hand]
  (= 2 (apply max (values hand))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (values hand))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (values hand))))

(defn flush? [hand]
  (= 5 (apply max (suits hand))))

(defn full-house? [hand]
  (= (range 2 4) (sort (values hand))))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort (values hand)))
      (= [1 4] (sort (values hand)))))

(defn straight? [hand]
  (let [card-values (map (fn [card] (rank card)) hand)
        ace-as-one (replace {14 1} (map (fn [card] (rank card)) hand))]
    (or (= (range (apply min card-values) (+ 1 (apply max card-values)))
           (sort card-values))
        (= (range (apply min ace-as-one) (+ 1 (apply max ace-as-one)))
           (sort ace-as-one)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hits (filter (fn [case] ((first case) hand)) checkers)]
    (apply max (map (fn [hit] (second hit)) hits))))
