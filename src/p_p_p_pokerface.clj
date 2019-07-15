(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
 (let [[rnk _] card]
   (if (Character/isDigit rnk)
     (Integer/valueOf (str rnk))
     (replacements rnk))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [freqs (frequencies (map rank hand))]
    (= 2 (apply max (vals freqs)))))

(defn three-of-a-kind? [hand]
  (let [freqs (frequencies (map rank hand))]
    (= 3 (apply max (vals freqs)))))

(defn four-of-a-kind? [hand]
  (let [freqs (frequencies (map rank hand))]
    (= 4 (apply max (vals freqs)))))

(defn flush? [hand]
  (let [suits (frequencies (map suit hand))]
    (= 1 (count (keys suits)))))

(defn full-house? [hand]
  (let [freqs (frequencies (map rank hand))
        ranks (vals freqs)]
    (and (= 2 (apply min ranks)) (= 3 (apply max ranks)))))

(defn two-pairs? [hand]
  (let [freqs (frequencies (map rank hand))]
    (= [1 2 2] (sort (vals freqs)))))

(defn straight? [hand]
  (let [normal (sort (map rank hand))
        lower-norm  (first normal)
        upper-norm  (inc (last normal))
        low-ace (sort (replace {14 1} normal))
        lower-ace  (first low-ace)
        upper-ace  (inc (last low-ace))]
    (or (= low-ace (range lower-ace upper-ace))
      (= normal (range lower-norm upper-norm)))
   ))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
    true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        handval  (fn [x] ((first x) hand))
        val      (fn [x] (second x)) ]
    (apply max (map val (filter handval checkers)))))


