(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(def low-repl {14 1})

(defn rank [card]
  (let [[rank _suit] card]
  (cond
   (Character/isDigit rank) (Integer/valueOf (str rank))
   :else (replacements rank))))

(defn suit [card]
  (let [[_rank suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        frequencies (vals (frequencies ranks))]
    (= (apply max frequencies) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        frequencies (vals (frequencies ranks))]
    (= (apply max frequencies) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        frequencies (vals (frequencies ranks))]
    (= (apply max frequencies) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        frequencies (vals (frequencies suits))]
	(= (apply max frequencies) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        frequencies (vals (frequencies ranks))
        sorted-frequencies (sort frequencies)]
     (= sorted-frequencies (range 2 4))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        frequencies (vals (frequencies ranks))
        sorted-frequencies (sort frequencies)]
     (= sorted-frequencies [1 2 2])))

(defn straight [start-value]
  (range start-value (+ start-value 5)))

(defn equals? [r s] (= (seq (straight s)) r))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
	start (first sorted-ranks)
	low-ranks (replace low-repl ranks)
	low-ace-ranks (sort low-ranks)
	low-ace-start (first low-ace-ranks)]
	(or
	 (equals? sorted-ranks start)
	 (equals? low-ace-ranks low-ace-start))))
    

(defn straight-flush? [hand]
  (and (flush? hand) 
       (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
	hand-values (map (fn [[checker value]]
		    	   (if (checker hand)
			     value
			     0))
			 checkers)]
    (apply max hand-values)))
    