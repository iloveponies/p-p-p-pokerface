(ns p-p-p-pokerface)

(def face-card-ranks
  {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get face-card-ranks r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn pair? [hand]
  (= 2 (some #{2} (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (some #{3} (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (some #{4} (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (some #{5} (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or (= [2 2] (filter 
                (fn [x] (> x 1)) 
                (vals (frequencies (map rank hand)))))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        low-card (first sorted-hand)]
    (or 
     (= sorted-hand (range low-card (+ low-card 5)))
     (= sorted-hand (conj (vec (range low-card (+ low-card 4))) 14)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply 
     max 
     (map second (filter (fn [x] ((first x) hand)) checkers)))))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
