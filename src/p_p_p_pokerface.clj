(ns p-p-p-pokerface)

(def replacements {\A 100, \B 20})

(defn rank [card]
  (let [[fst snd] card
        char->rank {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (cond
     (Character/isDigit fst)
     (Integer/valueOf (str fst))
     :else (get char->rank fst))))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn match-rank-frequencies [hand frequency]
  (->> hand
       (map #(rank %))
       (frequencies)
       (vals)
       (filter #(= frequency %))
       (empty?)
       (not)))

(defn pair? [hand]
  (match-rank-frequencies hand 2))

(defn three-of-a-kind? [hand]
  (match-rank-frequencies hand 3))

(defn four-of-a-kind? [hand]
  (match-rank-frequencies hand 4))

(defn flush? [hand]
  (let [num-cards (count hand)
        num-cards-suit (first (vals (frequencies (map #(suit %) hand))))]
    (= num-cards-suit num-cards)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (->> hand
       (map #(rank %))
       (frequencies)
       (vals)
       (filter #(= 2 %))
       (count)
       (= 2)))

(defn low-ace-hand [hand]
  (let [ranks (replace {\A, 1} (map #(first %) hand))
        suits (map #(second %) hand)]
    (for [x (map #(concat %) (map vector ranks suits))] (apply str x))))

(defn ace-hand-ranks [hand ace-val]
  (->> hand
       (map #(str (first %)))
       (replace {"A", ace-val})
       (map read-string)))

(defn high-ace-hand-ranks [hand]
  (ace-hand-ranks hand "14"))

(defn low-ace-hand-ranks [hand]
  (ace-hand-ranks hand "1"))

(defn straight? [hand]
  (let [high-ace-ranks (map #(rank %) hand)
        low-ace-ranks (replace {14, 1} high-ace-ranks)
        last-high (inc (apply max high-ace-ranks))
        last-low (inc (apply max low-ace-ranks))]
    (or
     (= (range (apply min low-ace-ranks) last-low) (sort low-ace-ranks))
     (= (range (apply min high-ace-ranks) last-high) (sort high-ace-ranks)))))

(defn straight-flush? [hand]
  nil)


(defn value [hand]
  nil)
