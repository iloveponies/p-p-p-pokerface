(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [ [r _] card]
    (if (not= nil (replacements r))
      (replacements r)
      (Integer/valueOf (str r)))))

(defn suit [card]
  (let [ [_ s] card]
    (str s)))

(defn pair? [hand]
  (let [cards (map rank hand)]
    (.contains (vals (frequencies cards)) 2)))

(defn three-of-a-kind? [hand]
  (let [cards (map rank hand)]
    (.contains (vals (frequencies cards)) 3)))

(defn four-of-a-kind? [hand]
  (let [cards (map rank hand)]
    (.contains (vals (frequencies cards)) 4)))

(defn flush? [hand]
  (let [cards (map suit hand)]
    (.contains (vals (frequencies cards)) 5)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [hand-ranks (map rank hand)]
    (= 2 (get (frequencies (vals (frequencies hand-ranks))) 2))))

(defn has-sequence? [sequence]
	(let [ordered-set (seq (sort sequence))
				first-value (first ordered-set)
				model-set (range first-value (+ first-value (count sequence)) )]
		(= ordered-set model-set)))

(defn straight? [hand]
  (let [hand-ranks (map rank hand)]
		(if (has-sequence? hand-ranks)
			true
			(has-sequence? (replace {14 1} hand-ranks)))))

(defn straight-flush? [hand]
	(and (straight? hand) (flush? hand)))


;; Pair	1
;; Two pairs	2
;; Three of a kind	3
;; Straight	4
;; Flush	5
;; High card (nothing)	0
;; Straight flush	8
;; Full house	6
;; Four of a kind	7

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
		:else 0))
