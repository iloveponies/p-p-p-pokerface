(ns p-p-p-pokerface)

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
