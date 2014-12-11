(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
  (if(Character/isDigit fst)
    (Integer/valueOf (str fst))
    (cond
      (= fst \T) 10
	  (= fst \J) 11
	  (= fst \Q) 12
	  (= fst \K) 13
	  (= fst \A) 14
	)
	)))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (let [numpairs (some #{2} (vals (frequencies (map rank hand))))]
  (not (or (= nil numpairs) (= numpairs 0)))))
  
(defn three-of-a-kind? [hand]
  (let [c (some #{3} (vals (frequencies (map rank hand))))]
  (not (or (= nil c) (= c 0)))))

(defn four-of-a-kind? [hand]
  (let [c (some #{4} (vals (frequencies (map rank hand))))]
  (not (or (= nil c) (= c 0)))))

(defn flush? [hand]
  (let [c (some #{5} (vals (frequencies (map suit hand))))]
  (not (or (= nil c) (= c 0)))))

(defn full-house? [hand]
  (let[[x y _] (vals (frequencies (map rank hand)))]
  (= (sort [x y]) [2 3])))
  
(defn two-pairs? [hand]
  (let[[x y _] (vals (frequencies (map rank hand)))]
  (or
    (= (sort [x y]) [2 2])
	(>= x 4)
	)))
	

(defn straight2? [hand]
  (let[x (sort (map rank hand))]
  (= x (range (first x) (+ (first x) 5)))))
  
(defn straight? [hand]
  (let[x (sort (map rank hand))]
    (or
	  (= x [2 3 4 5 14]) ;14 is an ace
	  (= x (range (first x) (+ (first x) 5)))
	  )))
  
;  (replace {14 2} [1 2 3 4]) ;=> ["a" "b" 3 4]
  ;(sort (map rank ["2H" "3S" "4C" "5D" "AD"]))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

  (defn high-card? [hand]
  true) ; All hands have a high card.
  
(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (let 
    [scores (map 
      (fn[x] (if ((first x) hand) (second x) -1) )
	  checkers)]
	  (apply max scores)
	  )))
