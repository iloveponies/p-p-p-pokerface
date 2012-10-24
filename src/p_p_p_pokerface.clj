(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        ch (first card)]
    (if (Character/isDigit ch)
      (Integer/valueOf (str ch))
      (replacements ch))))

(defn suit [card]
  (str (second card)))

(defn n-of-a-kind? [n hand]
  (let [ranks (map rank hand)]
  (>= (apply max (vals (frequencies ranks))) n)))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== 1 (count (set suits)))))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (and (== 2 (apply min (vals (frequencies ranks))))
         (== 3 (apply max (vals (frequencies ranks)))))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (cond
     (four-of-a-kind? hand) true
     (full-house? hand) true
     (and (not (three-of-a-kind? hand)) (== 3 (count (frequencies ranks)))) true
     :else false)))

(defn straight? [hand]
  (let [A-is-14-ranks (sort (map rank hand))
        A-is-1-ranks (sort (replace {14 1} A-is-14-ranks))
    	A-is-14-min (first A-is-14-ranks)
        A-is-1-min (first A-is-1-ranks)
        A-is-14-straight (range A-is-14-min (+ 5 A-is-14-min))
        A-is-1-straight (range A-is-1-min (+ 5 A-is-1-min))]
    (or (= A-is-14-ranks A-is-14-straight)
          (= A-is-1-ranks A-is-1-straight))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn hand-has-value? [hand value]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]]
    ((get checkers value) hand)))

(defn value [hand]
  (apply max (filter (fn [numb] (hand-has-value? hand numb)) (range 0 9))))