(ns p-p-p-pokerface)


(defn suit [card]
  (let [[_ s] card]
    (str s)))

(def face-value-map
  {\T 10
   \J 11
   \Q 12
   \K 13
   \A 14})

(defn rank [card]
  (let [[f _] card]
    (if (Character/isDigit f)
      (Integer/valueOf (str f))
      (get face-value-map f))))

(defn has-count-of-any-rank? [hand c]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (< 0 (count (filter (fn [x] (= x c)) freqs)))))

(defn pair? [hand]
  (has-count-of-any-rank? hand 2))


(defn three-of-a-kind? [hand]
  (has-count-of-any-rank? hand 3))


(defn four-of-a-kind? [hand]
  (has-count-of-any-rank? hand 4))


(defn flush? [hand]
  (let [suits (map suit hand)
        suit-freqs (vals (frequencies suits))]
    (= 5 (apply min suit-freqs))))

(defn sorted-rank-freqs [hand]
  (let [ranks (map rank hand)]
   (sort (vals (frequencies ranks)))))

(defn full-house? [hand]
  (let [rank-freq (sorted-rank-freqs hand)]
    (= [2  3] rank-freq)))


(defn two-pairs? [hand]
  (let [rf (sorted-rank-freqs hand)]
    (or
     (= [1 2 2] rf)
     (= [1 4] rf))))


(defn straight? [hand]
  (let [high-ace-rks (sort (map rank hand))
        low-ace-rks (sort (replace {14 1} high-ace-rks))
        straight-seq? (fn [seq]
                        (= (range (apply min seq) (+ 5 (apply min seq))) seq))]
    (or (straight-seq? high-ace-rks) (straight-seq? low-ace-rks))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        check-and-get-value (fn [c]
                              (if ((first c) hand)
                                (second c)
                                false))
        all-values (map check-and-get-value checkers)
        not-false-values (filter (fn [x] x) all-values)]
    (apply max not-false-values)))
