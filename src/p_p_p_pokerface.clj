(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        bigCards {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get bigCards r))))


(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn has-freq? [i t m]
  (let [f (fn [e] (== e i))]
    (not
      (empty?
        (filter
          f
          (vals (frequencies (map t m))))))))

;; How the whole thing works:

; 1) map cards in hand to ranks (=numeric values) and suits
(defn card-obj [card-str]
  {:rank (rank card-str), :suit (suit card-str)})

; 2) map entire hand to objects
(defn hand-obj [hand-strs]
  (let [cards (map card-obj hand-strs)
        freqs (map
                (fn [[freq matches]] [freq (map first matches)])
                (group-by
                  (fn [[_ v]] v)
                  (frequencies
                    (map
                      (fn [card] (get card :rank))
                      cards))))]
    {:cards cards, :freqs freqs}))

(defn has-n-freq? [frequency hits hand-str]
  (not
    (empty?
      (filter
        (fn [[freq ranks]] (and (== freq frequency) (== (count ranks) hits)))
        (get (hand-obj hand-str) :freqs)))))

(defn pair? [hand]
  (has-n-freq? 2 1 hand))

(defn three-of-a-kind? [hand]
  (has-n-freq? 3 1 hand))

(defn four-of-a-kind? [hand]
  (has-n-freq? 4 1 hand))

(defn flush? [hand]
  (= 5 (count (frequencies (map rank hand)))))

(defn full-house? [hand]
  (and (has-n-freq? 3 1 hand) (has-n-freq? 2 1 hand)))

(defn two-pairs? [hand]
  (or (has-n-freq? 4 1 hand) (has-n-freq? 2 2 hand)))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
