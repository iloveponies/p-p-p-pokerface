(ns p-p-p-pokerface)

(defn rank [card]
  (def values {\T 10, \J 11, \Q 12, \K 13, \A 14})
   (let [[fst _] card]
   (if (Character/isDigit fst)
   (Integer/valueOf (str fst))
   (values fst))))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (let [values (map rank hand)]
  ( > (apply max (vals (frequencies values))) 1)))

(defn three-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
  (and (== (apply max freqs) 3) (== (apply min freqs) 1))))

(defn four-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
  (and (== (apply max freqs) 4) (== (apply min freqs) 1))))

(defn flush? [hand]
  (let [suits (map suit hand)]
  (apply = suits)))

(defn full-house? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
  (and (== (apply max freqs) 3) (== (apply min freqs) 2))))

(defn two-pairs? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
  (and (== (first (next (next freqs))) 2) (== (first (next freqs)) 2))))

(defn straight? [hand]
  (let [values (sort (map rank hand)) 
  replc (sort (replace {14 1} values)) 
  biggest (+ (apply max values) 1) 
  smallest (apply min values) 
  bigrepl (+ (apply max replc) 1) 
  smarepl (apply min replc)]
  (and
   (or 
   (= values (range smallest biggest))
   (= replc (range smarepl bigrepl)))
   (not (flush? hand))
   )))

(defn straight-flush? [hand]
  (let [values (sort (map rank hand)) 
  replc (sort (replace {14 1} values)) 
  biggest (+ (apply max values) 1) 
  smallest (apply min values) 
  bigrepl (+ (apply max replc) 1) 
  smarepl (apply min replc)]
  (and
   (or 
   (= values (range smallest biggest))
   (= replc (range smarepl bigrepl)))
   (flush? hand)
   )))

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
