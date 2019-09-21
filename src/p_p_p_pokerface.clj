(ns p-p-p-pokerface)

(def characters {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [rank (get card 0)]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (characters rank))))

(defn suit [card]
  (str (get card 1)))

(defn in? [seq elem]
  "true if seq contains elem, false otherwise"
  (boolean (some #(= % elem) seq)))

(defn high-card? [hand]
  true)

(defn n-of-a-kind? [n f-type hand]
  (let [freq (frequencies (map f-type hand))]
    (in? (vals freq) n)))

(defn pair? [hand]
  (n-of-a-kind? 2 rank hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 rank hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 rank hand))

(defn flush? [hand]
  (n-of-a-kind? 5 suit hand))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (if (four-of-a-kind? hand)
      true
      (let [freq (vals (frequencies (map rank hand)))
            freq-freq (frequencies freq)]
        (= (freq-freq 2) 2))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        low-ace (sort (replace {14 1} ranks))
        card-range (fn [x] (range (apply min x) (+ (apply max x) 1)))]
    (if (in? ranks 2)
      (= low-ace (card-range low-ace))
      (= ranks (card-range ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand)
    (flush? hand)))

(def checkers #{[high-card? 0]  [pair? 1]
                [two-pairs? 2]  [three-of-a-kind? 3]
                [straight? 4]   [flush? 5]
                [full-house? 6] [four-of-a-kind? 7]
                [straight-flush? 8]})

(defn value [hand]
  (let [hands (filter #((first %) hand) checkers)]
    (apply max (map second hands))))
