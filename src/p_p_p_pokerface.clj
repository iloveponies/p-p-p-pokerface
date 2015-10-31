(ns p-p-p-pokerface)

(def any? (comp boolean some))

(defn rank [card]
  (let [[r s] card]
    (case r
      \T 10
      \J 11
      \Q 12
      \K 13
      \A 14
      (Integer/valueOf (str r)))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn group-by-rank [hand]
  (group-by rank hand))

(defn group-by-suite [hand]
  (group-by suit hand))

(defn get-rank-sequence [hand]
  (sort (map rank hand)))

(defn a-kind?
  ([hand nr] (a-kind? hand nr 1))
  ([hand nr times]
    (>= (count (filter #(= (count (val %)) nr) (group-by-rank hand))) times)))

(defn pair? [hand]
  (a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (a-kind? hand 4))

(defn flush? [hand]
  (= 1 (count (group-by-suite hand))))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (a-kind? hand 2 2))

(defn straight-helper? [hand]
  (let [sorted (get-rank-sequence hand)
        lowest (first sorted)
        straightrange (range lowest (+ lowest 5))]
    (= sorted straightrange)))

(defn straight? [hand]
  (or (straight-helper? hand)
      (straight-helper? (replace {"AH" "1H", "AD" "1D", "AC" "1C" "AS" "1S"} hand))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0))

