(ns p-p-p-pokerface)

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn rank [card]
  (let [[value _] card]
    (if (Character/isDigit value)
      (Integer/valueOf (str value))
      ({\T 10, \J 11, \Q 12, \K 13, \A 14} value))))

(defn suit [card]
  (let [[_ symbol] card]
    (str symbol)))

(defn ammounts [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (>= (apply max (ammounts hand)) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (ammounts hand)) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (ammounts hand)) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (ammounts hand) [2 3]))

(defn two-pairs? [hand]
  (or (= (ammounts hand) [1 2 2]) (= (ammounts hand) [1 4])))

(defn straight? [hand]
  (let [ranks            (sort (map rank hand))
        ranks-a-replaced (sort (replace {14 1} ranks))
        is-straight?     (fn [a-seq] (= a-seq [(first a-seq)
                                               (+ (first a-seq) 1)
                                               (+ (first a-seq) 2)
                                               (+ (first a-seq) 3)
                                               (+ (first a-seq) 4)]))]
    (or (is-straight? ranks) (is-straight? ranks-a-replaced))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))
