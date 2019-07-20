(ns p-p-p-pokerface)


(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card
        ch-first (str fst)]
    (if (Character/isDigit fst)
      (Integer/valueOf ch-first)
      (replacements fst))))


(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand))))
     1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand))))
     2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand))))
     3))

(defn flush? [hand]
  (== (first (vals (frequencies (map suit hand))))
     5))

(defn full-house? [hand]
  (= [2 3] (sort (vals
                   (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= '(2 2) (filter (fn [x] (= 2 x))
                   (sort (vals (frequencies (map rank hand)))))))

(defn rank-with-alphabets [card]
  (let [[fst _] card
        ch-first (str fst)]
    (if (Character/isDigit fst)
      (Integer/valueOf ch-first)
      fst)))

(defn straight? [hand]
  (let [seq-extracted (sort (map rank hand))
        tmp-lowest-element (apply min seq-extracted)
        seq-corrected-a (if (== 2 tmp-lowest-element)
                          (sort (replace {14 1} seq-extracted))
                          seq-extracted)
        lowest-element (apply min seq-corrected-a)
        highest-element (apply max seq-corrected-a)
        straight-seq (range lowest-element (+ 1 highest-element))]
     (= straight-seq seq-corrected-a)))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [checker] (if ((first checker) hand) (second checker) 0)) checkers))))
