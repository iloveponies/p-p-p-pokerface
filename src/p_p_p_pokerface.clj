(ns p-p-p-pokerface)

(def numeric-rank {\T 10,	\J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[hd & _] card]
    (if (Character/isDigit hd)
      (Integer/valueOf (str hd))
      (numeric-rank hd))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (let [frqs (vals (frequencies (map suit hand)))]
    (= (count frqs) 1)))

(defn full-house? [hand]
  (let [frqs (vals (frequencies (map rank hand)))]
    (or (= frqs '(3 2))
        (= frqs '(2 3)))))

(defn two-pairs? [hand]
  (let [frqs (vals (frequencies (map rank hand)))]
    (= (sort frqs) '(1 2 2))))

(defn straight? [hand]
  (letfn [(is-straight? [hand]
                        (let [ranks (sort (map rank hand))
                              min-rank (apply min ranks)
                              straight-range (range min-rank (+ min-rank 5))]
                          (= ranks straight-range)))
          (contains-A? [hand]
                       (> (count (filter (fn [x] (= x \A)) (map first hand))) 0))
          (replace-A [card]
                     (if (= (first card) \A)
                       (clojure.string/replace card \A \1)
                       card
                       ))]
    (if (contains-A? hand)
      (or
       (is-straight? (map replace-A hand))
       (is-straight? hand))
      (is-straight? hand))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        valid-hands (filter (fn [pair] ((first pair) hand)) checkers)
        scores (map second valid-hands)]
    (apply max scores)))
