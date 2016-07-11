(ns p-p-p-pokerface)

(def high-seven2                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand2                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand2               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand2         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand2          ["2H" "2S" "2C" "2D" "7D"])
(def straight2                     ["4H" "5S" "6C" "7D" "8D"])
(def straight-hand2                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand2        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand2       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand2                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand2              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand2          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand2  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand2 ["TS" "AS" "QS" "KS" "JS"])

(def rank-map {\1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14})

 ; in buffer use C-c M-n to change namespace
; C-c C-k to compile code to repl
(defn rank [param]
  (let [[r _] param] (rank-map r)))

(defn suit [card]
  (let [[_ c] card ] (str c)))

(defn x-of-a-kind? [x hand]
  (<= x (apply max (into [] (vals (frequencies (map rank hand)))))))

(defn hand-val->vector [hand]
  (into [] (sort (map rank hand))))  

(defn hand-freq->vector [hand] 
  (into [] (sort (vals (frequencies (map rank hand))))))

(defn average [hand]
  (let [values (hand-val->vector hand)
        aver (apply + values)
        length (count values)] 
    (quot aver length)))
  
(defn pair? [hand]
  (x-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (x-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (x-of-a-kind? 4 hand))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (= [2 3] (count (hand-freq->vector hand))))

(defn two-pairs? [hand]
  (= [1 2 2] (hand-freq->vector hand)))

(defn straight-helper [vector-hand]
  (let [vec vector-hand 
          len (count vec) 
          start (first vec)
          straight (take len (drop start (range)))]
    (= vec straight)))

(defn straight-flush? [hand]
  (let [vec (hand-val->vector hand)]
    (or (straight-helper (into [] (conj (filter #(> 14 %) vec) 1))) (straight-helper vec))))

(defn straight? [hand]
  (or (straight-helper (hand-val->vector hand)) (straight-flush? hand)))

(defn high-card? [hand]
  true)

 (defn value [hand]
   (let [checkers #{[high-card? 0]  [pair? 1]
                    [two-pairs? 2]  [three-of-a-kind? 3]
                    [straight? 4]   [flush? 5]
                    [full-house? 6] [four-of-a-kind? 7]
                    [straight-flush? 8]}
         check-hand (fn [checker] ((first checker) hand))]
     (let [hands-present (filter check-hand checkers)
           scores (map second hands-present)]
       (apply max scores))))

(println "straight?")
(println (straight? straight-hand2))
(println (straight? pair-hand2))
(println (straight-flush? low-ace-straight-flush-hand2))
(println (straight-flush? high-ace-straight-flush-hand2))
(println (straight-flush? straight-hand2))             ;=> false
(println (straight-flush? flush-hand2))               ;=> false
(println (straight-flush? straight-flush-hand2))     ;=> true
(println (straight-flush? low-ace-straight-flush-hand2)) ;=> true
(println (straight-flush? high-ace-straight-flush-hand2)) ;=> true





