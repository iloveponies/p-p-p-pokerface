(ns p-p-p-pokerface)

;; const-like

(defn high-card? [_]
  true) ; all hands have a high card

;; private
;; (concatenated asterisks in some fn names to break coupling with testing module + to introduce framework for inner data format)

(defn- card [a-card]
  a-card)

(defn- hand [a-hand]
  a-hand)

(defn- rank* [card]
  (let [ranks {\2 2
               \3 3
               \4 4
               \5 5
               \6 6
               \7 7
               \8 8
               \9 9
               \T 10
               \J 11
               \Q 12
               \K 13
               \A 14}
        [rank _] card]
    (get ranks rank)))

(defn- suit* [card]
  (let [[_ suit] card]
    (str suit)))

(defn- ranks-present-by-minimum-amount [hand amount]
  (->> hand
       (map rank*)
       frequencies
       (filter
         (fn [[_ freqs]]
           (>= freqs amount)))))

(defn- ranks-present-by-minimum-amount? [hand amount]
  (->> (ranks-present-by-minimum-amount hand amount)
       empty?
       not))

(defn- ranks-present-by-exact-amount? [hand amount]
  (->> hand
       (map rank*)
       frequencies
       (filter
         (fn [[_ freqs]]
           (= freqs amount)))
       empty?
       not))

(defn- pair?* [hand]
  (ranks-present-by-minimum-amount? hand 2))

(defn- three-of-a-kind?* [hand]
  (ranks-present-by-minimum-amount? hand 3))

(defn- four-of-a-kind?* [hand]
  (ranks-present-by-minimum-amount? hand 4))

(defn- flush?* [hand]
  (->> hand
       (map suit*)
       set
       count
       (= 1)))

(defn- full-house?* [hand]
  (and (ranks-present-by-exact-amount? hand 2)
       (ranks-present-by-exact-amount? hand 3)))

(defn- two-pairs?* [hand]
  (or (= 2 (count (ranks-present-by-minimum-amount hand 2)))
      (ranks-present-by-exact-amount? hand 4)))

(defn- straight?* [hand]
  (let [all-straight-combos
        #{#{14 2 3 4 5}
          #{2 3 4 5 6}
          #{3 4 5 6 7}
          #{4 5 6 7 8}
          #{5 6 7 8 9}
          #{6 7 8 9 10}
          #{7 8 9 10 11}
          #{8 9 10 11 12}
          #{9 10 11 12 13}
          #{10 11 12 13 14}}]
    (->> hand
         (map rank*)
         set
         (contains? all-straight-combos))))

(defn- straight-flush?* [hand]
  (and (straight?* hand)
       (flush?* hand)))

(defn- value* [hand]
  (let [checkers #{[high-card? 0]  [pair?* 1]
                   [two-pairs?* 2]  [three-of-a-kind?* 3]
                   [straight?* 4]   [flush?* 5]
                   [full-house?* 6] [four-of-a-kind?* 7]
                   [straight-flush?* 8]}
        scenarios-that-apply (filter (fn [checker]
                                       ((first checker) hand))
                                     checkers)
        all-applicable-points (map second scenarios-that-apply)]
    (apply max all-applicable-points)))

;; public

(defn rank [a-card]
  (rank* (card a-card)))
(defn suit [a-card]
  (suit* (card a-card)))
(defn pair? [a-hand]
  (pair?* (hand a-hand)))
(defn three-of-a-kind? [a-hand]
  (three-of-a-kind?* (hand a-hand)))
(defn four-of-a-kind? [a-hand]
  (four-of-a-kind?* (hand a-hand)))
(defn flush? [a-hand]
  (flush?* (hand a-hand)))
(defn full-house? [a-hand]
  (full-house?* (hand a-hand)))
(defn two-pairs? [a-hand]
  (two-pairs?* (hand a-hand)))
(defn straight? [a-hand]
  (straight?* (hand a-hand)))
(defn straight-flush? [a-hand]
  (straight-flush?* (hand a-hand)))
(defn value [a-hand]
  (value* (hand a-hand)))