(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [[fst __]]
  (Integer/valueOf (str (or (replacements fst) fst))))

(defn suit [[__ scnd]]
  (str scnd))

(defn max-freq-rank-hand [hand]
    (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (== (max-freq-rank-hand hand) 2))

(defn three-of-a-kind? [hand]
  (== (max-freq-rank-hand hand) 3))

(defn four-of-a-kind? [hand]
  (== (max-freq-rank-hand hand) 4))


(defn flush? [hand]
  (== 5 (first (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (seq (sort (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (= [1 2 2] (seq (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-set (into #{} ranks)
        ranks-set (if (and (contains? ranks-set 2)
                           (contains? ranks-set 14)) (replace {14 1} ranks-set)
                      ranks-set)
        sorted-ranks (sort ranks-set)
        min-rank (first sorted-ranks)]
    (= sorted-ranks (range min-rank (+ min-rank 5)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]},
        applfunc (fn [[func res]] (if (func hand) res))]
    (apply max (filter #(if (nil? %) false true) (map applfunc checkers)))))

