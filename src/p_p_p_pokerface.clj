(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        replacements {\A 14 \K 13 \Q 12 \J 11 \T 10}]
    (if (Character/isDigit fst) 
      (Integer/valueOf (str fst)) 
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (= [3 2] (seq (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (let [values (vals (frequencies (map rank hand)))
        freq (vals (frequencies (seq values)))]
    (or (= [2 1] (seq freq)) (= [1 1] (seq freq)))))

(defn straight? [hand]
  (let [hand-values (sort (map rank hand))
        test-straight (range (
                              first hand-values) 
                             (+ 1 (last hand-values)))
        hand-values2 (sort (replace {14 1} hand-values))
        test-straight2 (range (
                              first hand-values2) 
                             (+ 1 (last hand-values2)))]
    (or 
     (= (seq hand-values) (seq test-straight))
     (= (seq test-straight2)
        (seq hand-values2)))))

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

(defn high-card? [hand]
  (last (sort(map rank hand))))