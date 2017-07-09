(ns p-p-p-pokerface)

(defn rank [card]
  (let [ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [fst _] card]
    (if (Character/isDigit fst) 
      (Integer/valueOf (str fst)) 
      (get ranks fst))))
    

(defn suit [card]
  (let [[_ snd] card] 
    (str snd)))
  
(defn pair? [hand]
  (let [freqs (frequencies (map rank hand)) values (vals freqs)]
    (if (some  #{2} values)
     true 
     false)))
    
(defn three-of-a-kind? [hand]
  (let [freqs (frequencies (map rank hand)) values (vals freqs)]
    (if (some  #{3} values)
     true 
     false)))

(defn four-of-a-kind? [hand]
  (let [freqs (frequencies (map rank hand)) values (vals freqs)]
    (if (some  #{4} values)
     true 
     false)))

(defn flush? [hand]
  (let [freqs (frequencies (map suit hand)) values (vals freqs)]
    (if (== 5 (apply max values))
     true 
     false)))

(defn full-house? [hand]
  (if (and (pair? hand) (three-of-a-kind? hand))
   true 
   false))
   

(defn two-pairs? [hand]
  (let [freqs (frequencies (map rank hand)) values (sort (vals freqs))]
    (if (and (== (first values) 1) (or (== (second values) 2) (== (second values) 4)))
     true 
     false)))

(defn straight? [hand]
  (let [h1 (sort (map rank hand)) h2 (sort (replace {14 1} (map rank hand))) fst1 (first h1) fst2 (first h2)] 
    (or ( = (range fst1 (+ 5 fst1)) (seq h1)) (= (range fst2 (+ 5 fst2)) (seq h2)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) 

(defn value [hand] 
  (cond 
    (straight-flush? hand) 8 
      (four-of-a-kind? hand) 7
        (full-house? hand) 6 
          (flush? hand) 5 
            (straight? hand) 4
              (three-of-a-kind? hand) 3
                (two-pairs? hand) 2
                  (pair? hand) 1
                    (high-card? hand) 0))

