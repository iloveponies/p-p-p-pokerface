(ns p-p-p-pokerface)

(def picranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst) 
      (Integer/valueOf (str fst))
      (picranks fst))))

(defn suit [card]
  (str (let [[_ snd] card]
    snd)))

(defn pair? [hand]
  (== (count (filter #{2} (vec (vals (frequencies (map rank hand)))))) 1))

(defn three-of-a-kind? [hand]
  (== (count (filter #{3} (vec (vals (frequencies (map rank hand)))))) 1))

(defn four-of-a-kind? [hand]
  (== (count (filter #{4} (vec (vals (frequencies (map rank hand)))))) 1))

(defn flush? [hand]
  (== (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) '(2 3)))

(defn two-pairs? [hand]
  (let [freqs (vec (vals (frequencies (map rank hand))))]
   (or (== (count (filter #{2} freqs)) 2) (== (count (filter #{4} freqs))1))))

(defn straight-ranks? [ranks]
  (== 4 (- (apply max ranks) (apply min ranks))))

(defn straight? [hand]
  (let [ranks (map rank hand)] 
    (and
    (or (straight-ranks? ranks) (straight-ranks? (replace {14, 1} ranks)))
    (== (count (set ranks)) 5))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(def checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]})

(defn value [hand]
  (apply max (map second (filter
    (fn [checker] ((first checker) hand))
      checkers))))
