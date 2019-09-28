(ns p-p-p-pokerface)

;;some test hands
;;(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
;;(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
;;(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
;;(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
;;(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
;;(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
;;(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
;;(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
;;(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
;;(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
;;(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
;;(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
;;(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(def pic-cards { \T 10, \J 11, \Q 12, \K 13, \A 14 })

(defn high-card? [hand] true)

(defn rank 
	([card] (rank card pic-cards))
	([card pic-map]
	  (let [[fst _] card]
		(if (Character/isDigit fst) 
			(Integer/valueOf (str fst))
			(get pic-map fst)))))

(defn suit [card]
  (let [[_ snd] card] (str snd)))

(defn freq-vals [rank-or-suit hand]
	(let [f (frequencies (map rank-or-suit hand))]
		(vals f)))
			
(defn n-of-a-kind? [hand n]
  (>= (apply max (freq-vals rank hand)) n))

(defn pair? [hand]
	(n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
	(n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
	(n-of-a-kind? hand 4))

(defn flush? [hand]
  (= (apply max (freq-vals suit hand)) 5))

(defn full-house? [hand]
  	(let [fv (freq-vals rank hand)]
  		(= (range 2 4) (sort fv))))

(defn two-pairs? [hand]
  	(let [fv (freq-vals rank hand)]
  		(= [1 2 2] (sort fv))))

(defn aces-low-rank [card] (rank card (assoc pic-cards \A 1)))

(defn sorted-ranks [ranker hand]
	(sort (map ranker hand)))

(defn straight-range? [ranks]
	(let [fs (first ranks)
		rng (range fs (+ fs 5))]
		(= ranks rng)))

(defn straight? [hand]
  (let [r (sorted-ranks rank hand)
  		l (sorted-ranks aces-low-rank hand)]
  		(or (straight-range? r) (straight-range? l))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(def checkers #{
	[high-card? 0]
	[pair? 1]
	[two-pairs? 2]
	[three-of-a-kind? 3]
	[straight? 4]
	[flush? 5]
	[full-house? 6]
	[four-of-a-kind? 7]
	[straight-flush? 8]})

(defn filter-checkers [hand checker]
	(let [[checker-fn? _] checker]
		(checker-fn? hand)))

(defn value [hand]
  (let [matching-checkers (filter (partial filter-checkers hand) checkers)
  		matching-scores (map #(second %) matching-checkers)]
  		(apply max matching-scores)))
