(ns p-p-p-pokerface)

(defn rank [[val _]]
  (let [m {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit val)
      (Integer/valueOf (str val))
      (get m val) ) ) )

(defn suit [[value suit]]
  (str suit))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 1 (count (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
   (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn first-two-in-sequence? [lst]
  (= (+ 1 (first lst)) (second lst)))

(defn in-sequence? [lst]
  (or (< (count lst) 2)
      (and (first-two-in-sequence? lst)
           (in-sequence? (rest lst)))))

(defn straight? [hand]
  (let [ranks (sort (keys (frequencies (map rank hand)))),
        in? (fn [elm lst] (some #(= elm %) lst))
        ]
    (if (= 5 (count ranks))
      (or (in-sequence? ranks)
          (in-sequence? (sort (replace {14 1} ranks))) )
      false) ) )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
   true 0 ))
