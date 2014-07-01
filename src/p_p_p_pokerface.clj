(ns p-p-p-pokerface)

(defn rank [card]
  (let [face-cards {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [value _] card]
    (if (Character/isDigit value)
      (Integer/valueOf (str value))
      (face-cards value))))

(defn suit [card]
  (let [[_ sym] card]
    (str sym)))

(defn has-of-a? [number valueFn hand]
  (if (some #(= number %) (vals (frequencies (map valueFn hand))))
    true
    false))

(defn has-of-a-kind? [number hand]
  (has-of-a? number rank hand))


(defn has-of-a-suit? [number hand]
  (has-of-a? number suit hand))

(defn pair? [hand]
  (has-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (has-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (has-of-a-kind? 4 hand))

(defn flush? [hand]
  (has-of-a-suit? 5 hand))

(defn full-house? [hand]
  (and (has-of-a-kind? 2 hand) (has-of-a-kind? 3 hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (seq [1 2 2]) (sort (vals (frequencies (map rank hand)))))))


(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        hi-card (apply max sorted-hand)
        lo-card (apply min sorted-hand)
        has-ace? (= hi-card 14)
        all-unique? (every? #(= 1 %) (vals (frequencies sorted-hand)))]
    (if has-ace?
     (and all-unique? (or
                       (= (- 14 lo-card) 4)
                       (= (- (first (rest (reverse sorted-hand))) 1) 4)))
     (and all-unique? (= (- hi-card lo-card) 4)))))

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
   :else 0))
