(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        royals {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (cond
      (Character/isDigit fst)
        (Integer/valueOf (str fst))
      :else
       (get royals fst)
    )))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn sorted-ranks [hand]
  (sort (map (fn [card] (rank card)) hand)))

(defn sorted-values [hand]
  (sort (vals (frequencies (sorted-ranks hand)))))

(defn kind-of [hand, n]
  (= n (apply max (sorted-values hand))))

(defn pair? [hand]
  (kind-of hand 2))

(defn three-of-a-kind? [hand]
  (kind-of hand 3))

(defn four-of-a-kind? [hand]
  (kind-of hand 4))

(defn flush? [hand]
  (let [suites (map (fn [card] (suit card)) hand)]
    (apply = suites)))

(defn full-house? [hand]
  (let [ values (sorted-values hand) ]
    (and
      (= 2 (first values))
      (= 3 (second values)))))

(defn two-pairs? [hand]
  (let [ [_ snd thrd] (sorted-values hand) ]
    (and
      (= 2 snd)
      (= 2 thrd))))

(defn straight? [hand]
  (let [ ranks (sorted-ranks hand)
         low-ace-ranks (sort (replace {14 1} ranks))
         strght? (fn [sorted-ranks] (= sorted-ranks (range (first sorted-ranks) (+ (first sorted-ranks) 5))))]
    (or (strght? ranks) (strght? low-ace-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        filtered-checkers (filter (fn [checker] ((first checker) hand)) checkers)
        hand-values (map second filtered-checkers)]
    (apply max hand-values)))
