(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}
        digit? (fn [c] (Character/isDigit c))
        int-value (fn [c] (Integer/valueOf (str c)))
        [rnk _] card]
    (if (digit? rnk)
      (int-value rnk)
      (replacements rnk))))

(defn suit [card]
  (let [[_ suut] card]
    (str suut)))

(defn frequency-vals [aseq]
  (vals (frequencies aseq)))

(defn num-of-a-kind? [n hand]
  (let [ranks (map rank hand)]
    (<= n (apply max (frequency-vals ranks)))))

(defn pair? [hand]
  (num-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (num-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (num-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [hand-size (count hand)
        suits (map suit hand)]
    (== hand-size (apply max (frequency-vals suits)))))

(defn full-house? [hand]
  (let [full-house [2 3]
        ranks (map rank hand)]
    (= (sort (frequency-vals ranks))
       full-house)))

(defn two-pairs? [hand]
  (let [two-pairs [1 2 2]
        ranks (map rank hand)]
    (or (= two-pairs (sort (frequency-vals ranks)))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [card-ranks (sort (map rank hand))
        smallest-card (first card-ranks)
        straight (range smallest-card (+ smallest-card 5))
        low-ace-straight (range 1 6)
        low-ace-hand (sort (replace {14 1} card-ranks))]
    (or (= card-ranks straight)
        (= low-ace-hand low-ace-straight))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hand-types (filter #(apply (first %) [hand]) checkers)
        hand-scores (map second hand-types)]
  (apply max hand-scores)))
