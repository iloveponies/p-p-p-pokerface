(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [vals (vals (frequencies (map rank hand)))]
     (< 1 (apply max vals))))

(defn three-of-a-kind? [hand]
  (let [vals (vals (frequencies (map rank hand)))]
     (< 2 (apply max vals))))

(defn four-of-a-kind? [hand]
  (let [vals (vals (frequencies (map rank hand)))]
     (< 3 (apply max vals))))

(defn flush? [hand]
  (let [vals (vals (frequencies (map suit hand)))]
     (< 4 (apply max vals))))

(defn full-house? [hand]
  (let [vals (vals (frequencies (map rank hand)))]
  (= [2 3] (sort vals))))

(defn two-pairs? [hand]
  (let [vals (vals (frequencies (map rank hand)))]
    (or (= [1 2 2] (sort vals)) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [high-ace-ranks (sort (map rank hand))
        min-high-ace (apply min high-ace-ranks)
        max-high-ace (apply max high-ace-ranks)
        high-range (range min-high-ace (+ 1 max-high-ace))
        high-straight (= high-ace-ranks high-range)
        low-ace-ranks (sort (replace {14 1} (map rank hand)))
        min-low-ace (apply min low-ace-ranks)
        max-low-ace (apply max low-ace-ranks)
        low-range (range min-low-ace (+ 1 max-low-ace))
        low-straight (= low-ace-ranks low-range)]
    (or low-straight high-straight)))

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
