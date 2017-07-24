(ns p-p-p-pokerface)

(defn rank [[r _]]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (cond
      (Character/isDigit r) (- (int r) (int \0))
      :else (get replacements r)
    )))


(defn suit [[_ s]]
  (str s))

(defn count_cards [hand cnt criteria]
  (count (filter
           (fn [number_of_cards] (= cnt number_of_cards))
           (vals (frequencies (map criteria hand))))))

(defn pair? [hand]
  (> (count_cards hand 2 rank) 0))
  ;;(> (count (filter (fn [cnt] (= cnt 2)) (vals (frequencies (map rank hand))))) 0))

(defn three-of-a-kind? [hand]
  (> (count_cards hand 3 rank) 0))

(defn four-of-a-kind? [hand]
  (> (count_cards hand 4 rank) 0))

(defn flush? [hand]
  (> (count_cards hand 5 suit) 0))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= (count_cards hand 2 rank) 2))



(defn straight? [hand]
  (or (let [
         ranks (sort (map rank hand))
         start (first ranks)
         end (+ start 5)
       ]
    (= ranks (range start end)))
      (let [
         ranks (sort (replace {14 1} (map rank hand)))
         start (first ranks)
         end (+ start 5)
       ]
    (= ranks (range start end)))
  ))


(defn high-card? [hand]
  true)

(defn straight-flush? [hand]
  (and
         (straight? hand)
         (flush? hand)
  ))

(defn value [hand]
  (let [matchers [
                    [(high-card? hand)       0]
                    [(pair? hand)            1]
                    [(two-pairs? hand)       2]
                    [(three-of-a-kind? hand) 3]
                    [(straight? hand)        4]
                    [(flush? hand)           5]
                    [(full-house? hand)      6]
                    [(four-of-a-kind? hand)  7]
                    [(straight-flush? hand)  8]
           ]
          ]
         (apply max (map second (filter #(first %) matchers)))
    ))
