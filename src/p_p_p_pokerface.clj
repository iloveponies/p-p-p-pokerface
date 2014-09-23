(ns p-p-p-pokerface)

(def rank-map
  {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn in?
  "true if seq contains elm"
  [a-seq elm]
  (some #(= elm %) a-seq))

(defn rank
  "Card is a string whose first character is the rank, and the second is the suit."
  [[rank _]]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (get rank-map rank)))

(defn suit [[_ suit]]
  (str suit))

(defn number-occurrences-of-group-with-size-n
  "Returns the number of time a group of size n occurs in the hand."
  [hand n]
  (let [ranks (map rank hand)
        rank-counts (frequencies ranks)
        group-sizes (vals rank-counts)
        group-size-counts (frequencies group-sizes)]
    (get group-size-counts n 0)))

(defn number-occurrences-of-suits
  "returns a map of suit to its number of occurences"
  [hand]
  (let [suits (map suit hand)]
    (frequencies suits)))

(defn pair? [hand]
  (= 1 (number-occurrences-of-group-with-size-n hand 2)))

(defn three-of-a-kind? [hand]
  (= 1 (number-occurrences-of-group-with-size-n hand 3)))

(defn four-of-a-kind? [hand]
  (= 1 (number-occurrences-of-group-with-size-n hand 4)))

(defn flush? [hand]
  (not (not (in? (vals (number-occurrences-of-suits hand)) 5))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [pair-count (number-occurrences-of-group-with-size-n hand 2)
        four-count (number-occurrences-of-group-with-size-n hand 4)]
    (or (= pair-count 2) (= four-count 1))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        high-ace-ranks (sort ranks)
        low-ace-ranks (sort (replace {14 1} ranks))
        smallest-high-ace (first high-ace-ranks)
        smallest-low-ace (first low-ace-ranks)]
    (or
      (= (range smallest-low-ace (+ 5 smallest-low-ace)) low-ace-ranks)
      (= (range smallest-high-ace (+ 5 smallest-high-ace)) high-ace-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        applicable (filter (fn [[checker _]] (checker hand)) checkers)
        points (map second applicable)]
    (apply max points)))

