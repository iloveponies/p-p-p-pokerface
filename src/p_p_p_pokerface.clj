(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}
        rnk (first card)]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (get replacements rnk))))

(defn- freqs [hand]
  (vals (frequencies (map rank hand))))

(defn- has-freq? [n hand]
  (boolean (some #{n} (freqs hand))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (has-freq? 2 hand))

(defn three-of-a-kind? [hand]
  (has-freq? 3 hand))

(defn four-of-a-kind? [hand]
  (has-freq? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (and (has-freq? 2 hand)
       (has-freq? 3 hand)))

(defn two-pairs? [hand]
  (or (= (count (filter #{2} (freqs hand))) 2)
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks (sort (mapv rank hand))
        low (first ranks)
        high (last ranks)]
    (or (and (= 4 (- high low)) (= 5 (count (freqs hand))))
        (= ranks [2 3 4 5 14])
        (= ranks [10 11 12 13 14]))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [checkers [pair? two-pairs? three-of-a-kind? straight? flush? full-house? four-of-a-kind? straight-flush?]
        hits-and-misses (map #(% hand) checkers)
        hits-and-misses-scored (zipmap hits-and-misses (range 1 9))]
    (apply max (map (fn [[hit? score]] (if hit? score 0)) hits-and-misses-scored))))

