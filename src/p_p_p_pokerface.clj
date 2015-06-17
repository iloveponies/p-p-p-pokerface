(ns p-p-p-pokerface)


(def character-rank 
  {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn is-digit [v]
  (Character/isDigit v))

(defn to-digit [v]
  (Integer/valueOf (str v)))

(def not-nil? (complement nil?))

(defn rank [card]
  (let [[fst _] card]
    (if (is-digit fst) 
      (to-digit fst)
      (character-rank fst))))

(defn ranks-of-a-hand [hand] (map rank hand))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn occurrences [hand] 
  (vals (frequencies (ranks-of-a-hand hand))))

(defn pair? [hand]
  (let [occurrences (occurrences hand)]
    (not-nil? (some #(= 0 (mod % 2)) occurrences))))

(defn n-of-a-kind? [n hand]
  (let [occurrences (occurrences hand)]
    (= n (apply max occurrences))))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= 1 (count (set suits)))))

(defn full-house? [hand]
  (let [occurrences (occurrences hand)
        suits (map suit hand)]
   (and 
    (= #{3 2} (set occurrences))
    (= 4 (count (set suits))))))

(defn two-pairs? [hand]
  (let [occurrences (occurrences hand)]
    (= 2 (count (filter #(= 2 %) occurrences)))))

(defn straight? [hand]
  (let [ranks (set (map #(mod (mod % 13) 8) (ranks-of-a-hand hand)))
        start (apply min ranks)
        end   (inc (apply max ranks))
        rng   (set (range start end))]
    (and
     (= 5 (count ranks))
     (= ranks rng))))

(defn straight-flush? [hand]
  (and
   (flush? hand)
   (straight? hand)))

(def table [[pair? 1] 
            [two-pairs? 2]
            [three-of-a-kind? 3]
            [straight? 4]
            [flush? 5]
            [full-house? 6]
            [four-of-a-kind? 7]
            [straight-flush? 8]
            [not-empty 0]])

(defn value [hand]
   (apply max (for [mapping table
           :let [[func point] mapping]
           :when (func hand)]
       point)))

