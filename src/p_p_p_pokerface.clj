(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      (= \T rank) 10
      (= \J rank) 11
      (= \Q rank) 12
      (= \K rank) 13
      (= \A rank) 14)))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn same-n? [hand count]
  (boolean (some #{count} (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (same-n? hand 2))

(defn three-of-a-kind? [hand]
  (same-n? hand 3))

(defn four-of-a-kind? [hand]
  (same-n? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freqs (frequencies (vals (frequencies (map rank hand))))]
    (or (= (get freqs 2) 2) (= (get freqs 4) 1))))

(defn is-straight? [ranks]
  (let [sorted-ranks (sort ranks)
        min-rank (first sorted-ranks)]
    (= sorted-ranks (range min-rank (+ min-rank 5)))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (or
     (is-straight? ranks)
     (is-straight? (replace {14 1} ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
