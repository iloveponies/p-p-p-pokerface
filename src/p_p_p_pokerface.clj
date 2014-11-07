(ns p-p-p-pokerface)
(def highrankmap {\T 10,
                  \J 11,
                  \Q 12,
                  \K 13
                  \A 14})
(defn rank [card]
  (let [r (let [[r _] card] r)]
    (if (Character/isDigit r) 
      (Integer/valueOf (str r))
      (highrankmap r))))


(defn suit [card]
  (str (let [[_ s] card] s)))

(defn n-of-a-kind? [n]
  #(>= (apply max (vals (frequencies (map rank %)))) n))

(defn pair? [hand]
  ((n-of-a-kind? 2) hand))

(defn three-of-a-kind? [hand]
  ((n-of-a-kind? 3) hand))

(defn four-of-a-kind? [hand]
  ((n-of-a-kind? 4) hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (and (= [2 3] freqs))))

(defn two-pairs? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (and (= [1 2 2] freqs))))


(defn straight? [hand]
  (let [flo (apply min (map rank hand))
        ranks (sort (map rank hand))
        target [0 1 2 3 4]
        recenter (fn[n] #(- % n))]
    (or (= target (map (recenter flo) ranks))
    (= target (map (recenter 1) (sort (replace {14 1} ranks)))))))


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
  (apply max (map #(if ((first %) hand) (second %) 0) checkers))))

