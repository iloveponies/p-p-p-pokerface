(ns p-p-p-pokerface)

(def rank->integer {\T 10,
                    \J 11,
                    \Q 12,
                    \K 13,
                    \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (rank->integer r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))


(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= (apply max freqs) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= (apply max freqs) 3)))


(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= (apply max freqs) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (= (apply max freqs) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= (set freqs) #{2,3})))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (or
      (= freqs [1,2,2])
      (= freqs [1,4]))))

(defn straight? [hand]
  (let [ranks-ace-high (map rank hand)
        ranks-ace-low (replace {14 1} ranks-ace-high)
        low-1 (apply min ranks-ace-high)
        low-2 (apply min ranks-ace-low)]
    (or
      (= (sort ranks-ace-high) (range low-1 (+ low-1 5)))
      (= (sort ranks-ace-low) (range low-2 (+ low-2 5))))))



(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))



(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{ [high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8] }
        filtered (filter (fn [tuple] ((first tuple) hand)) checkers)
        values (map second filtered)]
    (apply max values)))



