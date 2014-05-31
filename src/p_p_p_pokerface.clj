(ns p-p-p-pokerface)

(def rank-of-char
 {\2 2
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
  \A 14})

(def rank-of-char-low
 {\A 1
  \2 2
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
  \K 13})

(defn rank [card]
  (let [[r s] card]
    (if (Character/isDigit r) (Integer/valueOf (str r)))
    (rank-of-char r)))

(defn rank-low [card]
  (let [[r s] card]
    (if (Character/isDigit r) (Integer/valueOf (str r)))
    (rank-of-char-low r)))

(defn suit [card]
  (str (second card)))

(defn- of-a-kind-frequencies [hand]
  (let [ranks (map rank hand)]
    (-> ranks
        frequencies
        vals
        frequencies)))

(defn- of-a-kind [hand n]
  (-> hand
      of-a-kind-frequencies
      (contains? n)))

(defn pair? [hand]
  (of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (of-a-kind hand 4))

(defn flush? [hand]
  (->> hand
       (map suit)
       frequencies
       count
       (== 1)))

(defn full-house? [hand]
  (let [freqs (of-a-kind-frequencies hand)]
    (and (contains? freqs 3)
         (contains? freqs 2))))

(defn two-pairs? [hand]
  (let [freqs (of-a-kind-frequencies hand)
        pair-count (get freqs 2)]
    (== 2 (or pair-count 0))))

(defn straight?
  ([hand rank]
   (let [ranks (map rank hand)]
     (-> ranks
         sort
         (#(= % (range (first %) (+ (first %) 5)))))))

  ([hand]
    (or (straight? hand rank)
        (straight? hand rank-low))))

(defn straight-flush? [hand]
  (boolean
    (and (straight? hand)
         (flush? hand))))

(defn value [hand]
  (condp #(%1 %2) hand
    straight-flush?  8
    four-of-a-kind?  7
    full-house?      6
    flush?           5
    straight?        4
    three-of-a-kind? 3
    two-pairs?       2
    pair?            1
                     0))
