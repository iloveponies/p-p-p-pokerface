(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        s {\T 10, \J 11, \Q 12, \K 13, \A 14}] 
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get s r))))

(defn suit [card]
  (let [[_ s] card] (str s)))

(defn freq [hand]
  (->> hand (map rank) frequencies vals (apply max)))

(defn pair? [hand]
  (= (freq hand) 2))

(defn three-of-a-kind? [hand]
  (= (freq hand) 3))

(defn four-of-a-kind? [hand]
  (= (freq hand) 4))

(defn flush? [hand]
  (= (->> hand (map suit) set count) 1))

(defn full-house? [hand]
  (= (->> hand (map rank) frequencies vals) '(3 2)))

(defn two-pairs? [hand]
  (= (->> hand (map rank) frequencies vals (filter #(> % 1))) '(2 2)))

(defn straight? [hand]
  (let [f (->> hand (map rank) sort)
        r (if (< (first f) 10) (sort (replace {14 1} f)) f)
        fst (first r)]
    (= r (range fst (+ fst 5)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                  [two-pairs? 2] [three-of-a-kind? 3]
                  [straight? 4]  [flush? 5]
                  [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]}]
    (apply max (map (fn [[fst snd]] (if (fst hand) snd -1)) checkers))))
