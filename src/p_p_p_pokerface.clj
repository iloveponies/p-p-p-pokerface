(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (cond
    (= rank \T) 10
    (= rank \J) 11
    (= rank \Q) 12
    (= rank \K) 13
    (= rank \A) 14
    :else       (Integer/valueOf (str rank))))

(defn suit [[_ suit]]
  (str suit))

(defn count-of-a-kind? [hand c & {:keys [num] :or {num 1}}]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        kinds (filter #(= % c) (vals freqs))]
    (= (count kinds) num)))

(defn pair? [hand]
  (count-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (count-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (count-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (set (map suit hand))]
    (= (count suits) 1)))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or 
    (count-of-a-kind? hand 2 :num 2)
    (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks-low-ace (sort (replace {14 1} ranks))]
    (or
      (= ranks (range (first ranks) (inc (last ranks))))
      (= ranks-low-ace (range (first ranks-low-ace) (inc (last ranks-low-ace)))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (->> checkers
         (map (fn [x] [((first x) hand) (second x)]))
         (filter #(true? (first %)))
         (map second)
         (apply max))))
