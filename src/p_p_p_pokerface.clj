(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        rank_map {\T 10 \J 11 \Q 12 \K 13 \A 14}
        numeric? (Character/isDigit rank)]
    (if numeric? (Integer/valueOf (str rank))
                 (rank_map rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-freqs [hand]
  (let [_ranks (map rank hand)
        _freq (frequencies _ranks)
        _vals (vals _freq)]
    _vals))

(defn of-a-kind? [hand _count]
  (boolean (some #{_count} (rank-freqs hand))))

(defn pair? [hand]
  (of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (of-a-kind? hand 4))

(defn flush? [hand]
  (let [_suits (map suit hand)
        _freq (frequencies _suits)
        _count (count _freq)]
    (= 1 _count)))

(defn sorted-rank-freqs [hand]
  (sort (rank-freqs hand)))

(defn full-house? [hand]
  (= [2 3] (sorted-rank-freqs hand)))

; If there are two pairs - the last card is a single occurence
(defn two-pairs? [hand]
  (= [1 2 2] (sorted-rank-freqs hand)))

(defn straight? [hand]
  (let [_ranks (sort (map rank hand))
        _ace? (= 14 (last _ranks))
        _two? (= 2 (first _ranks))
        ; If the hand contains an ace and two,
        ; replace the trailing ace with
        ; the leading one
        _ranks (if (and _two? _ace?)
                 (cons 1 (take 4 _ranks))
                 _ranks)
        _first (first _ranks)
        _expected (range _first (+ _first 5))
        ]
    (= _ranks _expected)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  nil)
