(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[rank-char _] card]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (replacements rank-char))))

(defn suit [card]
  (let [[_ suit-char] card] (str suit-char)))

(defn freq [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (= '(1 1 1 2) (freq hand)))

(defn three-of-a-kind? [hand]
  (= '(1 1 3) (freq hand)))

(defn four-of-a-kind? [hand]
  (= '(1 4) (freq hand)))

(defn full-house? [hand]
  (= '(2 3) (freq hand)))

(defn two-pairs? [hand]
  (= '(1 2 2) (freq hand)))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        replaced-a (sort (replace {14 1} sorted-ranks))
        correct-range? (fn [x] (= 4 (- (last x) (first x))))]
    (and (= '(1 1 1 1 1) (freq hand))
         (some true? (map correct-range? [sorted-ranks replaced-a])))))

(defn straight-flush? [hand]
  (true? (and (flush? hand) (straight? hand))))

(defn value [hand]
  (let [functions [high-card? pair? two-pairs? three-of-a-kind? straight?
                   flush? full-house? four-of-a-kind? straight-flush?]
        values (range 9)]
    (last (map first
               (filter second
                       (map list
                            values
                            (map #(% hand) functions)))))))
