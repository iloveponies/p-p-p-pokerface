(ns p-p-p-pokerface)

(defn rank [card]
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

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn ranks-present-by-minimum-amount [hand amount]
  (->> hand
       (map rank)
       frequencies
       (filter
         (fn [[_ freqs]]
           (>= freqs amount)))))

(defn ranks-present-by-minimum-amount? [hand amount]
  (->> (ranks-present-by-minimum-amount hand amount)
       empty?
       not))

(defn ranks-present-by-exact-amount? [hand amount]
  (->> hand
       (map rank)
       frequencies
       (filter
         (fn [[_ freqs]]
           (= freqs amount)))
       empty?
       not))

(defn pair? [hand]
  (ranks-present-by-minimum-amount? hand 2))

(defn three-of-a-kind? [hand]
  (ranks-present-by-minimum-amount? hand 3))

(defn four-of-a-kind? [hand]
  (ranks-present-by-minimum-amount? hand 4))

(defn flush? [hand]
  (->> hand
       (map suit)
       set
       count
       (= 1)))

(defn full-house? [hand]
  (and (ranks-present-by-exact-amount? hand 2)
       (ranks-present-by-exact-amount? hand 3)))

(defn two-pairs? [hand]
  (or (= 2 (count (ranks-present-by-minimum-amount hand 2)))
      (ranks-present-by-exact-amount? hand 4)))

(defn straight? [hand]
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
         (map rank)
         set
         (contains? all-straight-combos))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

;; goto tutorial

(defn value [hand]
  nil)
