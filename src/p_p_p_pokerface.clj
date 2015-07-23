(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card]
    (if (Character/isDigit rnk)
       (Integer/valueOf (str rnk))
       ({\T 10, \J 11, \Q 12, \K 13, \A 14} rnk))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (= 5 (first freqs))))


(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (and (contains? (set freqs) 3)
         (contains? (set freqs) 2))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        ffreqs (vals (frequencies freqs))]
    (or (contains? (set ffreqs) 2)
        (four-of-a-kind? hand))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
