(ns p-p-p-pokerface)

(def ranks
  {\T 10
   \J 11
   \Q 12
   \K 13
   \A 14})

(defn rank [card]
  (let [[r, _] card]
	(if (Character/isDigit r)
	  (Integer/valueOf (str r))
	  (ranks r))))


(defn suit [card]
  (let [[_, su] card]
	(str su)))

(defn matches [hand]
  (let [digits (map rank hand)
		freqs (frequencies digits)]
	(vals freqs)))

(defn n-of-a-kind [n hand]
  (let [freqs (matches hand)
		top (apply max freqs)
		valid (<= n top)]
	(boolean valid)))

(defn pair? [hand]
  (n-of-a-kind 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)
		valid (apply = suits)]
  (boolean valid)))

(defn sorted-tops [hand]
  (let [freqs (matches hand)]
	(sort freqs)))

(defn full-house? [hand]
	(let [tops (sorted-tops hand)
		  valid (= tops [2, 3])]
	(boolean valid)))

(defn two-pairs? [hand]
  (let [tops (sorted-tops hand)
		valid (or (= tops [1, 2, 2])
				  (= tops [1, 4])
				  (= tops [2, 3]))]
	(boolean valid)))

(defn straight? [hand]
  (defn straight?-helper [digits]
	(let [sorted-digits (sort digits)
		  lower (first sorted-digits)
		  valid-digits (range lower (+ lower 5))]
		(= sorted-digits valid-digits)))
  (let [digits-a (map rank hand)
		digits-b (replace {14 1} digits-a)
		valid (or (straight?-helper digits-a)
				  (straight?-helper digits-b))]
	(boolean valid)))

(defn straight-flush? [hand]
  (and (straight? hand)
	   (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
		check (fn [checker]
				(let [check-fn (first checker)]
				  (check-fn hand)))
		passed (filter check checkers)
		values (map second passed)]
	(apply max values)))


(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
