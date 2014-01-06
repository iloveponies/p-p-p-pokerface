(ns p-p-p-pokerface)

(def high-ranks {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [[first _]]
  (if (Character/isDigit first)
    (Integer/valueOf (str first))
    (high-ranks first)))

(defn suit [[_ snd]]
  (str snd))

(defn rank-counts [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn high-card? [hand] true)

(defn n-of-a-kind? [hand n]
  (let [rank-count-set (set (rank-counts hand))]
    (contains? rank-count-set n)))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-counts (set (vals (frequencies suits)))]
    (contains? suit-counts 5)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (let [freqs (rank-counts hand)]
        (= [1 2 2] (sort freqs)))))

(defn straight? [hand]
  (let [ranks-high-ace (sort (map rank hand))
        ranks-low-ace (sort (replace {14 1} ranks-high-ace))

        low-card-with-high-ace (apply min ranks-high-ace)
        low-card-with-low-ace (apply min ranks-low-ace)

        high-ace-straight (range low-card-with-high-ace
                                 (+ 5 low-card-with-high-ace))

        low-ace-straight (range low-card-with-low-ace
                                (+ 5 low-card-with-low-ace))]

    (or (= ranks-high-ace high-ace-straight)
        (= ranks-low-ace low-ace-straight))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4]  [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}

        all-values (map
                    (fn [checker] (if ((first checker) hand)
                                   (second checker)
                                   0))
                    checkers)]

    (apply max all-values)))
