(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r s]      card
        face-cards {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get face-cards r))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn pair? [hand]
  (let [x (frequencies (map rank hand))
        n (count (filter #{2} (vals x)))]
    (= n 1)))

(defn three-of-a-kind? [hand]
  (let [x (frequencies (map rank hand))
        n (count (filter #{3} (vals x)))]
    (= n 1)))

(defn four-of-a-kind? [hand]
  (let [x (frequencies (map rank hand))
        n (count (filter #{4} (vals x)))]
    (= n 1)))

(defn flush? [hand]
  (let [x (frequencies (map suit hand))]
    (contains? (set (vals x)) 5)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [x (frequencies (map rank hand))
        n (count (filter #{2} (vals x)))]
    (= n 2)))

(defn straight? [hand]
  (let [ranked-hand (map rank hand)
        sorted-hand (sort (cond
                           (contains? (set ranked-hand) 13) ranked-hand
                           (contains? (set ranked-hand) 2) (replace {14 1} ranked-hand)
                           :else ranked-hand))
        min-card (apply min sorted-hand)
        base-hand (range min-card (+ min-card 5))]
    (= base-hand sorted-hand)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
  (->> (filter #((first %1) hand) checkers)
       (map second)
       (apply max))))
