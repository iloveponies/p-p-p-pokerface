(ns p-p-p-pokerface)

(defn rank [card]
  (let [t {\T 10
           \J	11
           \Q 12
           \K 13
           \A 14}
        [fst snd] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get t fst))))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn maxrankcount [hand]
  (apply max (vals
        (frequencies
            (map
               rank
               hand)))))

(defn pair? [hand]
  (= (maxrankcount hand) 2))

(defn three-of-a-kind? [hand]
  (= (maxrankcount hand) 3))

(defn four-of-a-kind? [hand]
  (= (maxrankcount hand) 4))

(defn maxsuitcount [hand]
  (apply max (vals (frequencies (map suit hand)))))

(defn flush? [hand]
  (= (maxsuitcount hand) 5))

(defn full-house? [hand]
  (let [sorted (sort (vals (frequencies (map rank hand))))]
    (= (seq [2 3]) sorted)))

(defn two-pairs? [hand]
  (let [sorted (sort (vals (frequencies (map rank hand))))]
    (or
     (four-of-a-kind? hand)
     (= (seq [1 2 2]) sorted))))

;(replace {1 "a", 2 "b"} [1 2 3 4]) ;=> ["a" "b" 3 4]
;["2H" "3S" "4C" "5D" "AD"]
;["TH" "AS" "QC" "KD" "JD"]
(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        minrank (apply min sorted-ranks)
        template-straight (range minrank (+ minrank 5))]
   (or
      (= sorted-ranks template-straight)
      (= (range 1 6) (sort (replace {14 1} sorted-ranks))))))

(defn straight-flush? [hand]
  (and
   (flush? hand)
   (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        is-type? (fn [checker] ((first checker) hand))]
    (apply max (map second (filter is-type? checkers)))))
