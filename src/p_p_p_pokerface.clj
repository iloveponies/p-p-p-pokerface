(ns p-p-p-pokerface)

(defn rank [card]
  (let [[s-rank _] card
        rank-map { \T 10 \J 11 \Q 12 \K 13 \A 14 }
        h-rank (get rank-map s-rank)]
    (if h-rank
      h-rank
      (Integer/valueOf (str s-rank)))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn count-ranks [hand cut-level]
  (filter 
    (fn [freqq] (let [[rank freq] freqq]
                  (> freq cut-level)))
    (frequencies (map rank hand))))

(defn pair? [hand]
  (not (empty? (count-ranks hand 1))))

(defn three-of-a-kind? [hand]
  (not (empty? (count-ranks hand 2))))

(defn four-of-a-kind? [hand]
  (not (empty? (count-ranks hand 3))))

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
