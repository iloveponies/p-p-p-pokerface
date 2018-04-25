(ns p-p-p-pokerface)

(def replacements {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14})

; exercises
; all can be done in many diffferent ways
(defn rank [card]
  (let [[r _] card]
    (get replacements r)))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn frequencies-counts-by [hand f]
  (vals (frequencies (map f hand))))

(defn fcs-by-rank [hand]
  (frequencies-counts-by hand rank))

(defn fcs-by-suit [hand]
  (frequencies-counts-by hand suit))

(defn pair? [hand]
  (let [fcs (fcs-by-rank hand) mx (apply max fcs) c (count fcs)]
    (and (== 2 mx) (== 4 c))))

(defn three-of-a-kind? [hand]
  (let [fcs (fcs-by-rank hand) mx (apply max fcs) mn (apply min fcs)]
    (and (== 3 mx) (== 1 mn))))

(defn four-of-a-kind? [hand]
  (let [fcs (fcs-by-rank hand) mx (apply max fcs)]
    (== 4 mx)))

(defn flush? [hand]
  (let [fcs (fcs-by-suit hand) mx (apply max fcs)]
    (== 5 mx)))

(defn full-house? [hand]
  (let [fcs (sort (fcs-by-rank hand))]
    (= (range 2 (+ 2 2)) fcs)))

(defn two-pairs? [hand]
  (let [fcs (fcs-by-rank hand) mx (apply max fcs) c (count fcs)]
    (and (== 2 mx) (== 3 c))))

(defn straight? [hand]
  (let [r (sort (replace replacements (map rank hand)))
        fst (first r)
        lst (last r)]
    (if (and (== 2 fst) (== 14 lst))
      (= (range 2 6) (filter (fn [x] (not= 14 x)) r))
      (= (range fst (+ fst 5)) r))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))

