(ns p-p-p-pokerface)

(defn rank [card]
  (let [[^Character rank] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-frequencies [hand]
  (vals
    (frequencies (map rank hand))))

(defn n-of-a-kind [hand n]
  (if (some (fn [freq] (= n freq)) (rank-frequencies hand))
    true
    false))

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (set '(2 3)) (set (rank-frequencies hand))))

(defn two-pairs? [hand]
  (= 2 (count (filter (fn [freq] (= 2 freq)) (rank-frequencies hand)))))

(defn straight? [hand]
  (let [normalized (fn [ranks]
                     (let [smallest-rank (apply min ranks)
                           sorted-ranks (sort ranks)]
                       (map (fn [rank] (- rank smallest-rank)) sorted-ranks)))
        ranks (map rank hand)
        alt-ranks (replace {14 1} ranks)
        normalized-ranks (normalized ranks)
        normalized-alt-ranks (normalized alt-ranks)
        normalized-straight (range 0 5)]
    (or (= normalized-straight normalized-ranks)
        (= normalized-straight normalized-alt-ranks))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

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
