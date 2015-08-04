(ns p-p-p-pokerface)

(defn rank [card]
  (let [[c _] card
        face {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (cond
     (Character/isDigit c) (Integer/valueOf (str c))
     :else (get face c))))

(defn suit [card]
  (let [[_ s] card] (str s)))

(defn pair? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (contains? (set counts) 2)))

(defn three-of-a-kind? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (contains? (set counts) 3)))

(defn four-of-a-kind? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (contains? (set counts) 4)))

(defn flush? [hand]
  (let [counts (vals (frequencies (map suit hand)))]
    (contains? (set counts) 5)))

(defn full-house? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (= [2 3] (sort counts))))

(defn two-pairs? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (or (= [1 2 2 ] (sort counts)) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        adjRanks (map (fn [x] (- x (first ranks))) ranks)
        expect1 (range 0 5)
        expect2 (replace {4 12} expect1)]
    (or (= expect1 adjRanks) (= expect2 adjRanks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1] [two-pairs? 2]
                   [three-of-a-kind? 3] [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        handVal (fn [pair]
            (let [fun (first pair)
                  val (second pair)]
              (if (fun hand) val)))
        values (map handVal checkers)
        ]
    (apply max (filter #(not (nil? %)) values))))
