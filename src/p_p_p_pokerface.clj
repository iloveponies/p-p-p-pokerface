(ns p-p-p-pokerface)

(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})
(defn rank [card]
  (let [[r,_] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

; Helper method so I don't have to copy-paste this and manage to typo
(defn of-kind? [hand n]
  ;(contains? (vals (frequencies (map rank hand))) n))
  ; contains? not supported on vals() return type
  ;(< 0 (+ (vals (filter (fn [x] (== x n)) (map rank hand))))))
  ;(< 0 (get (frequencies (map rank hand)) n)))
  (<= n (apply max (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (of-kind? hand 2))

(defn three-of-a-kind? [hand]
  (of-kind? hand 3))

(defn four-of-a-kind? [hand]
  (of-kind? hand 4))

(defn flush? [hand]
  (apply = (mapv suit hand)))

; Got tired of writing it multiple times
(defn freq-vals [hand]
  (vals (frequencies (map rank hand))))

(defn full-house? [hand]
  ; only 5 cards, only possibility is that sorted, we have [2 3]
  (= [2 3] (sort (freq-vals hand))))

(defn two-pairs? [hand]
  ; only possibility when sorted: [1 2 2], or [1 4]
  ; either way, product is 4
  (== 4 (apply * (freq-vals hand))))

;;;;;;;;;;;;; DONE TO HERE ;;;;;;;;;;;;;
; ace replacement map
(def acer {14 1})
; adjust by the first element (so I don't have to write multiple times and
; for some reason, I didn't want to put it into the let declarations
(defn adjust-shandr [shandr]
  ;(-  shandr (first shandr)))
  (let [f (first shandr)]
    (map (fn [x] (- x f)) shandr)))

;(defn in? [arr v]
;  (let [single (== 1 (count arr))
;        fequal (== v (first arr))]
;    (if single
;      fequal
;      (or fequal (in? (rest arr) v)))))
;  (or (== v (first arr)) (if (== 1 (count arr)) false (in? (rest arr)))))

(defn straight? [hand]
  ; when subtracting the first number, must be equivalent to [0 1 2 3 4]
  (let [straight [0 1 2 3 4]
        shandr (sort (map rank hand))
        seq1 (adjust-shandr shandr)
        ; if we have an ace, put it at the front as well
        ; let replace do all the work
        seq2 (adjust-shandr (sort (replace acer shandr)))]
    (or (= straight seq1) (= straight seq2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)
(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))
; above works, but do it according to the assignment, later
;  (let [checkers #{ [high-card? 0]  [pair? 1]
;                    [two-pairs? 2]  [three-of-a-kind? 3]
;                    [straight? 4]   [flush? 5]
;                    [full-house? 6] [four-of-a-kind? 7]
;                    [straight-flush? 8]}
;        filfn (fn [p? b x] (if (p? x
;    (apply max (filter checkers hand))))
;    ;(map (fn [x] (- x f)) shandr)))
