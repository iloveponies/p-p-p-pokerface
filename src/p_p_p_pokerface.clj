(ns p-p-p-pokerface)


(def replacement {"T" 10 "J" 11 "Q" 12 "K" 13 "A" 14})
;(defn rank [card] (let [ [r _] card] (if (Character/isDigit r) (Integer/valufeOf r) (replacement (str r)))))
(defn rank [card] (let [ [r _] card] (if (Character/isDigit r) (Integer/valueOf (str r)) (replacement (str r)))))


(defn suit [card] (let [ [_ scnd] card] (str scnd)))

(defn pair? [hand] (if(> ( apply max (vals (frequencies  (map (fn [x] (let [[n s] x] n)) hand )))) 1) true false))


(defn three-of-a-kind? [pair] (contains? (set(vals (frequencies (map (fn [one] (let [[num lettr] one] (str num)))  pair)))) 3))


(defn four-of-a-kind?[pair] (contains? (set(vals (frequencies (map (fn [one] (let [[num lettr] one] (str num)))  pair)))) 4))

(defn flush? [hand] (= 1 (count (keys (frequencies (map (fn [pair] (let [ [n f] pair] (str f))) hand))))))

 (defn full-house? [hand] (let [fre  (frequencies (map (fn [pair] (let [[n h] pair] (str n))) hand))] (if (or (> (count (keys fre)) 2) (< (count (keys fre)) 2)) false (if (and (= (apply + (vals fre)) 5) (not (nil? (some #{3} (vals fre))))) true false))))



(defn two-pairs? [hand] (> (count (filter (fn [x] (>= x 2)) (vals (frequencies (map rank hand))))) 1)) 


;(defn two-pairs? [hand] (let [first-group
                ;                    (frequencies (map (fn [x] (let [[n s] x] s)) hand))] 
                 ;                (let [max-symbol  (key (apply max-key val first-group)) ]  
                  ;                    (let [candidate-map (filter (fn [h] (let [[ n s] h] 
                   ;                          (not (= s max-symbol)))) hand) 
                    ;                         real-map (filter (fn [x1] (let [ [n1 s1] x1](= s1 max-symbol))) hand )] 
                     ;                         (let [number (map (fn [f] ( let [[x1 y1] f] x1)) real-map)]
                      ;                            (not (empty? (filter (fn [x] (let [[xm ym] x] (not (nil? (some #{xm} number))))) candidate-map))))))))


(defn straight? [hand] (let [map-number (map rank hand) min-card (apply min map-number) max-card (apply max map-number)] (if (and (> (- max-card min-card) 4) (= max-card 14)) (let [card-array  (filter (fn [x] (not (= x max-card))) map-number) max-arr (apply max card-array)] (= (range min-card (+ max-arr 1)) (sort card-array))) (= (sort map-number) (range min-card (+ max-card 1))))))

(defn straight-flush? [hand] (and (flush? hand) (straight? hand)))
(defn high-card? [hand]
  true) ; 

(defn value [hand] ( let [checkers #{[high-card? 0]  [pair? 1]  [two-pairs? 2]  [three-of-a-kind? 3] [straight? 4]   [flush? 5]  [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}]  (apply max (filter (fn [x] (not (nil? x))) (map (fn [f] (if ((first f) hand) (second f))) checkers)))))
