(ns p-p-p-pokerface)

(def card-map {
               "1" 1
               "2" 2
               "3" 3
               "4" 4
               "5" 5
               "6" 6
               "7" 7
               "8" 8
               "9" 9
               "T" 10
               "J" 11
               "Q" 12 
               "K" 13 
               "A" 14 
               })
#_(do
    (def high-seven ["2H" "3S" "4C" "5C" "7D"])
    (def pair-hand ["2H" "2S" "4C" "5C" "7D"])
    (def two-pairs-hand ["2H" "2S" "4C" "4D" "7D"])
    (def three-of-a-kind-hand ["2H" "2S" "2C" "4D" "7D"])
    (def four-of-a-kind-hand ["2H" "2S" "2C" "2D" "7D"])
    (def straight-hand ["2H" "3S" "6C" "5D" "4D"])
    (def low-ace-straight-hand ["2H" "3S" "4C" "5D" "AD"])
    (def high-ace-straight-hand ["TH" "AS" "QC" "KD" "JD"])
    (def flush-hand ["2H" "4H" "5H" "9H" "7H"])
    (def full-house-hand ["2H" "5D" "2D" "2C" "5S"])
    (def straight-flush-hand ["2H" "3H" "6H" "5H" "4H"])
    (def low-ace-straight-flush-hand ["2D" "3D" "4D" "5D" "AD"])
    (def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"]))


(defn rank [card]
  (let [[rank _] card]
    (get card-map (str rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)) )

#_(defn pair? [hand]
  (= 1 (count (filter #(= 2 %) (vals (frequencies (map rank hand)))))))

(defn pair? [hand]
  (->> (map rank hand)
       (frequencies)
       (vals)
       (filter #(= 2 %))
       (count)
       (= 1)))

(def hand-value {
                 :pair {2 1}                                ;value = {number how-many-times}
                 :two-pairs {2 2}
                 :three-of-a-kind {3 1}                     ;in frequency distribution
                 :four-of-a-kind {4 1}
                 :straight {}
                 
                 })

(def hand  ["2H" "2S" "4C" "5C" "7D"])

#_(pair? pair-hand)  ;=> true
#_(pair? high-seven) ;=> false


(defn three-of-a-kind? [hand]
  (->> (map rank hand)
       (frequencies)
       (vals)
       (filter #(= 3 %))
       (count)
       (= 1)))

#_(three-of-a-kind? two-pairs-hand)       ;=> false
#_(three-of-a-kind? three-of-a-kind-hand) ;=> true

(defn four-of-a-kind? [hand]
    (->> (map rank hand)
         (frequencies)
         (vals)
         (filter #(= 4 %))
         (count)
         (= 1)))


#_(four-of-a-kind? two-pairs-hand)      ;=> false
#_(four-of-a-kind? four-of-a-kind-hand) ;=> true

(defn two-pairs? [hand]
  (or
    (->> (map rank hand)
         (frequencies)
         (vals)
         (filter #(= 2 %))
         (count)
         (= 2))
    (four-of-a-kind? hand)))

#_(two-pairs? two-pairs-hand)      ;=> true
#_(two-pairs? pair-hand)           ;=> false
#_(two-pairs? four-of-a-kind-hand) ;=> true



(defn flush? [hand]
  (->> (map suit hand)
       (frequencies)
       (vals)
       (filter #(= 5 %))
       (count)
       (= 1)))

#_(flush? pair-hand)  ;=> false
#_(flush? flush-hand) ;=> true)

(defn full-house? [hand]
  (->> (map rank hand)
       (frequencies)
       (vals)
       (set)
       (#(and (contains? % 2)
              (contains? % 3)))))
#_(full-house? three-of-a-kind-hand) ;=> false
#_(full-house? full-house-hand)      ;=> true

#_(two-pairs? two-pairs-hand)      ;=> true
#_(two-pairs? pair-hand)           ;=> false
#_(two-pairs? four-of-a-kind-hand) ;=> true

(defn straight? [hand]
  (let [rank-vec (vec (map rank hand))
        max-rank (apply max rank-vec)
        min-rank (apply min rank-vec)
        max-diff (- max-rank min-rank)
        sum-rank (reduce + rank-vec)]
    (and (->> rank-vec 
              (frequencies)
              (vals)
              (filter #(= 1 %))
              (count)
              (= 5))
         (or (= 4 max-diff)
             (and
               (= 14 max-rank)
               (= 2 min-rank)
               (= 28 sum-rank))))))

#_(straight? two-pairs-hand)             ;=> false
#_(straight? straight-hand)              ;=> true
#_(straight? low-ace-straight-hand)      ;=> true
#_(straight? ["2H" "2D" "3H" "4H" "5H"]) ;=> false
#_(straight? high-ace-straight-hand)     ;=> true


(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

#_(straight-flush? straight-hand)                ;=> false
#_(straight-flush? flush-hand)                   ;=> false
#_(straight-flush? straight-flush-hand)          ;=> true
#_(straight-flush? low-ace-straight-flush-hand)  ;=> true
#_(straight-flush? high-ace-straight-flush-hand) ;=> true

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (->> (map #(let [[func val] %]
                (if (func hand) val 0))
              checkers)
         (apply max))))


#_(value high-seven)           ;=> 0
#_(value pair-hand)            ;=> 1
#_(value two-pairs-hand)       ;=> 2
#_(value three-of-a-kind-hand) ;=> 3
#_(value straight-hand)        ;=> 4
#_(value flush-hand)           ;=> 5
#_(value full-house-hand)      ;=> 6
#_(value four-of-a-kind-hand)  ;=> 7
#_(value straight-flush-hand)  ;=> 8
