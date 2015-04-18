(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        rankmap {\A 14, \K 13, \Q 12, \J 11, \T 10}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (Integer/valueOf (str (get rankmap fst))))))

(defn suit [card]
  (let [[_ sec] card]
    (str sec)))

(defn freqs [func hand]
  "Turned commonly used sequence into a small helper function."
  (vals (frequencies (map func hand))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (== (apply max (freqs rank hand)) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (freqs rank hand)) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (freqs rank hand)) 4))

(defn flush? [hand]
  (== (apply max (freqs suit hand)) 5))

(defn full-house? [hand]
  (= (sort (freqs rank hand)) [2 3]))

(defn two-pairs? [hand]
  (and (= (sort (freqs rank hand)) [1 2 2])
       (= (sort (freqs suit hand)) [1 1 1 2])))

(defn straight? [hand]
  (let [check-seq #(= % (range (first %) (inc (last %))))
        mrank     (map rank hand)]

    (or (check-seq (sort mrank))
        (check-seq (sort (replace {14 1} mrank))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value-seq [hand]
  "Takes in a 'hand' and returns a sequence containing valid hand values."
  (for [checks #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]

   (if ((first checks) hand)
     (second checks) 0)))

(defn value [hand]
   (apply max (value-seq hand)))
