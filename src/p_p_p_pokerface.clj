(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fsd scd] card
        get-value (fn [] fsd)
        isdigit? (fn [x] (Character/isDigit x))
        convi (fn [x] (cond 
                       (isdigit? x) (Integer/valueOf (str x))
                       :else ({"T" 10, "J" 11, "Q" 12, "K" 13, "A" 14} (str x))))]
    (convi (get-value))))

(defn suit [card]
  (let [[fst scd] card
        sec (fn [] scd)]
   (str (sec))))

(defn pair? [hand]
  (let [values (fn [] (map rank hand))
        freq (fn [] (vals (frequencies (values))))
        ispair? (fn [] 
                   (cond
                     (> (count (filter (fn [x] (== x 2)) (freq))) 0) true
                     :else  false))]
  	(ispair?)))

(defn three-of-a-kind? [hand]
  (let [values (fn [] (map rank hand))
        freq (fn [] (vals (frequencies (values))))
        isthree? (fn [] 
                   (cond
                     (> (count (filter (fn [x] (== x 3)) (freq))) 0) true
                     :else  false))]
  	(isthree?)))

(defn four-of-a-kind? [hand]
  (let [values (fn [] (map rank hand))
        freq (fn [] (vals (frequencies (values))))
        isfour? (fn [] 
                   (cond
                     (> (count (filter (fn [x] (== x 4)) (freq))) 0) true
                     :else  false))]
  	(isfour?)))

(defn flush? [hand]
  (let [values (fn [] (map suit hand))
        insp (fn [] (apply = (values)))
        is (fn [] 
              (cond
                (insp) true
                :else false))]
    (is)))

(defn full-house? [hand]
  (let [values (fn [] (map rank hand))
        freq (fn [] (vals (frequencies (values))))
        isfull? (fn [] 
                   (if (== (count (filter (fn [x] (== x 3)) (freq))) 1)
                     (if (== (count (filter (fn [x] (== x 2)) (freq))) 1)
                       true
                       false)
                     false))]
  	(isfull?)))

(defn two-pairs? [hand]
  (let [values (fn [] (map rank hand))
        freq (fn [] (vals (frequencies (values))))
        ispair? (fn [] 
                   (cond
                     (== (count (filter (fn [x] (== x 2)) (freq))) 2) true
                     (== (count (filter (fn [x] (== x 4)) (freq))) 1) true
                     :else  false))]
  	(ispair?)))

(defn straight? [hand]
  (let [values (fn [] (map rank hand))
        make-ref (fn [x] (range x (+ x 5)))
        insp (fn [] (= (sort (values)) (make-ref (first (sort (values))))))
        insp-two (fn [] (if (== (last (sort (values))) 14)
                           (= (sort (replace {14 1} (values))) (make-ref 1))
                           false))
        is (fn [] 
              (cond
                (insp) true
                (insp-two) true
                :else false))]
    (is)))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))
  
(defn high-card? [hand] true)

(defn value [hand]
	(let [checkers #{[high-card? 0]  [pair? 1]
		             [two-pairs? 2]  [three-of-a-kind? 3]
		             [straight? 4]   [flush? 5]
		             [full-house? 6] [four-of-a-kind? 7]
		             [straight-flush? 8]}
		  extract (fn [x] (apply max (map second x)))
		  walk (fn [] (extract (filterv (fn [x] ((get x 0) hand)) checkers)))]
      (walk)))
