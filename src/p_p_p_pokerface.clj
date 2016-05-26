(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13 \A 14})
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))


(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (boolean (and (some #{2} (vals (frequencies ranks)))
         (some #{3} (vals (frequencies ranks)))))))

(defn two-pairs? [hand]
  (let [sorted_ranks (sort (map rank hand))]
    (= [2 2 1] (vals (frequencies sorted_ranks)))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        high_ace (sort ranks)
        low_ace (sort (replace {14 1} ranks))]
    (or (apply = 1 (map - (rest high_ace) high_ace))
        (apply = 1 (map - (rest low_ace) low_ace)))))

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
    (apply max (map (fn [checker]
            (if ((first checker) hand)
              (second checker)
              0)) checkers))))
