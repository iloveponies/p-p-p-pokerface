(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card
        face-card-values {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (face-card-values rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (let [rank-freqs (frequencies (map rank hand))
        freqs-only (vals rank-freqs)
        eq-2 (fn [freq] (>= freq 2))]
    (not (empty? (filter eq-2 freqs-only)))))

(defn three-of-a-kind? [hand]
  (let [rank-freqs (frequencies (map rank hand))
        freqs-only (vals rank-freqs)
        eq-3 (fn [freq] (>= freq 3))]
    (not (empty? (filter eq-3 freqs-only)))))

(defn four-of-a-kind? [hand]
  (let [rank-freqs (frequencies (map rank hand))
        freqs-only (vals rank-freqs)
        eq-4 (fn [freq] (>= freq 4))] ; probably should be eq-4
    (not (empty? (filter eq-4 freqs-only)))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [rank-freqs (frequencies (map rank hand))
        freqs-only (vals rank-freqs)
        sorted-freqs (sort freqs-only)]
    (= sorted-freqs [2 3])))

(defn two-pairs? [hand]
  (let [rank-freqs (frequencies (map rank hand))
        freqs-only (vals rank-freqs)
        sorted-freqs (sort freqs-only)]
    (or (= [1 2 2] sorted-freqs)
        (= [1 4] sorted-freqs))))


(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        alt-sorted-ranks (sort (replace {14 1} ranks)) ; alt value for ace
        in-sequence? (fn [s] 
                       (let [f (first s)] 
                         (= s (range f (+ f 5)))))]
    (or (in-sequence? sorted-ranks) 
        (in-sequence? alt-sorted-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[pair? 1] ; no need for high-card as no match = 0
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check-hand-against (fn [[ck v]] (if (ck hand) v 0))]
    (apply max (map check-hand-against checkers))))
