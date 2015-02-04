(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-conversions {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [r _] card
        rank-string (if (Character/isDigit r)
                      (str r)
                      (str (get rank-conversions r)))]
    (Integer/valueOf rank-string)))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn list-contains? [xs x]
  (> (count (filter (fn [y] (= y x)) xs)) 0))

(defn of-a-kind? [hand n]
  (list-contains? (vals (frequencies (map rank hand))) n))

(defn pair? [hand]
  (of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (of-a-kind? hand 4))

(defn flush? [hand]
  (list-contains? (vals (frequencies (map suit hand))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (get (frequencies (vals (frequencies (map rank hand)))) 2) 2)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ace-high-ranks (sort ranks)
        ace-low-ranks (sort (replace {14 1} ranks))
        is-straight? (fn [rs]
                       (= (map (fn [x] (- x (first rs))) rs)
                          '(0 1 2 3 4)))]
    (or (is-straight? ace-high-ranks)
        (is-straight? ace-low-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
;;     :else 0))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))

