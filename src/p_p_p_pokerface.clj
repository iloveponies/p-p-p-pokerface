(ns p-p-p-pokerface)


(defn rank [card]
  (let [[rank _] card
        hiranks {\T 10,
                 \J 11,
                 \Q 12,
                 \K 13,
                 \A 14}]
    (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (hiranks rank))))

;=============================================

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

;=============================================

(defn pair? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (= 2 (apply + (filter (fn [x] (> x 1)) ranks)))))

;=============================================

(defn three-of-a-kind? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (= 3 (apply + (filter (fn [x] (> x 1)) ranks)))))

;=============================================

(defn four-of-a-kind? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (= 4 (apply + (filter (fn [x] (= x 4)) ranks)))))

;=============================================

(defn flush? [hand]
  (let [suits (vals (frequencies (map suit hand)))]
    (apply = suits)))

;=============================================

(defn full-house? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (= 5 (apply + (filter (fn [x] (> x 1)) ranks)))))

;=============================================

(defn two-pairs? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (= 4 (apply + (filter (fn [x] (> x 1)) ranks)))))

;=============================================

(defn straight? [hand]
  (let [ranks (sort (replace {14 1, 10 2, 11 3, 12 4, 13 5} (map rank hand)))
        checker (range (first ranks) (+ 5 (first ranks)))]
    (= (seq ranks)
       (seq checker))))

;=============================================

(defn straight-flush? [hand]
  (let [suits (keys (frequencies (map suit hand)))]
    (and (= 1 (count suits))
         (straight? hand))))

;=============================================

(defn high-card? [hand]
  (let [ranks (frequencies (map rank hand))
        filtered-ranks (filter (fn [x] (= 1 (second x))) ranks)]
    (= 5 (count filtered-ranks))))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (remove nil? (map (fn [checker] (if
                         ((first checker) hand)
                          (second checker))) checkers)))))
