(ns p-p-p-pokerface)
(declare high-card?)

(defn rank [card]
  (let [[r _] card
        replacement {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacement r))))


(defn suit [card]
  (let [[_ s] card]
    (str s)))


(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

;; (defn pair2? [hand]
;;   (contains? (->> hand
;;                   (map rank)
;;                   frequenciesx
;;                   vals
;;                   set) 2))


(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))


(defn four-of-a-kind? [hand]
 (= (apply max (set (vals (frequencies (map rank hand))))) 4))

(defn flush? [hand]
  (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) '(2 3)))

(defn two-pairs? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (or (= freqs '(1 2 2))
        (= freqs '(1 4)))))


(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        first (first sorted-ranks)]
    (or (= sorted-ranks (range first (+ 5 first)))
        (= sorted-ranks (concat (range first (+ 4 first)) [14])))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

;; (defn value [hand]
;;   (cond
;;     (straight-flush? hand) 8
;;     (four-of-a-kind? hand) 7
;;     (full-house? hand) 6
;;     (flush? hand) 5
;;     (straight? hand) 4
;;     (three-of-a-kind? hand) 3
;;     (two-pairs? hand) 2
;;     (pair? hand) 1
;;     (high-card?) 0))


(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [[checker _]] (checker hand)) checkers)))))


(defn high-card? [hand]
  true)
