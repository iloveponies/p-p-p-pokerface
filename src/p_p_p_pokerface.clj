(ns p-p-p-pokerface)

(defn rank [card]
  (let [v (first card)
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
  (if (Character/isDigit v)
    (Integer/valueOf (str v))
    (replacements v))))

(defn suit [card]
  (str (second card)))

(defn many-kinds [n]
  (fn [hand]
    (-> (map rank hand) frequencies vals set (contains? n))))

(defn pair? [hand]
  ((many-kinds 2) hand))

(defn three-of-a-kind? [hand]
  ((many-kinds 3) hand))

(defn four-of-a-kind? [hand]
  ((many-kinds 4) hand))

(defn flush? [hand]
  (->> (map suit hand) set count (= 1)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
  (->> (map rank hand) frequencies vals sort (= '(1 2 2)))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        alt (replace {14 1} ranks)
        m (apply min ranks)
        helper (fn [r]
                 (let [m (apply min r)]
                    (= (sort r) (seq (range m (+ 5 m))))))]
  (or (helper ranks) (helper alt))))


(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))
