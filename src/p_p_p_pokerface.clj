(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-char (first card)]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (case rank-char
        \T 10
        \J 11
        \Q 12
        \K 13
        \A 14))))

(defn suit [card]
  (str (second card)))

(defn -rank-histogram [hand]
  "Counts the rank occurence in the hand and returns
   those as a map, with the key being the count of cards
   with the same rank, and the value being the number of
   occurences in this hand."
  (let [ranks (map rank hand)]
    (frequencies (vals (frequencies ranks)))))

(defn -suit-histogram [hand]
  (let [suits (map suit hand)]
    (frequencies (vals (frequencies suits)))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (let [rf (-rank-histogram hand)]
    (contains? rf 2)))

(defn three-of-a-kind? [hand]
  (let [rf (-rank-histogram hand)]
    (contains? rf 3)))

(defn four-of-a-kind? [hand]
  (let [rf (-rank-histogram hand)]
    (contains? rf 4)))

(defn flush? [hand]
  (let [sf (-suit-histogram hand)]
    (contains? sf 5)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [rf (-rank-histogram hand)]
    (or (= (get rf 2) 2)
        (four-of-a-kind? hand))))

(defn -consecutive? [ranks]
  (let [ordered-ranks (sort ranks)
   smallest (first ordered-ranks)]
   (= ordered-ranks (range smallest (+ smallest (count ranks))))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-ace-as-one (replace {14 1} ranks)]
    (or (-consecutive? ranks)
        (-consecutive? ranks-ace-as-one))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        checked (filter #((first %) hand) checkers)
        values (map second checked)]
    (apply max values)))


