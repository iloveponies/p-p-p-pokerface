(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get ranks r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (contains? (set (vals (frequencies suits))) 5)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (or (contains? (set freqs) 4)
        (= (get (frequencies freqs) 2) 2))))

(defn straight? [hand]
  (let [sorted-ace-high (sort (map rank hand))
        sorted-ace-low (sort (replace {14 1} sorted-ace-high))
        straight-ace-high (range (first sorted-ace-high) (inc (last sorted-ace-high)))
        straight-ace-low (range (first sorted-ace-low) (inc (last sorted-ace-low)))]
    (or (= sorted-ace-high straight-ace-high)
        (= sorted-ace-low straight-ace-low))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [tests #{[high-card? 0]  [pair? 1]
                [two-pairs? 2]  [three-of-a-kind? 3]
                [straight? 4]   [flush? 5]
                [full-house? 6] [four-of-a-kind? 7]
                [straight-flush? 8]}
        passing-tests (filter (fn [t] ((first t) hand)) tests)]
    (apply max (map second passing-tests))))

; ------
