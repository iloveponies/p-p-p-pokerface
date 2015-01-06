(ns p-p-p-pokerface)

(defn suit [card]
  (let [[_ s] card] (str s)))

(defn rank [card] 
  (let [[r _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r) (Integer/valueOf (str r)) 
     (get replacements r))))

(defn pair? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (if (nil? (some #(= % 2) counts)) false true)))

(defn three-of-a-kind? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (if (nil? (some #(= % 3) counts)) false true)))

(defn four-of-a-kind? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (if (nil? (some #(= % 4) counts)) false true)))

(defn flush? [hand]
  (let [counts (first (vals (frequencies (map suit hand))))]
    (= counts (count hand))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        counts (vals (frequencies ranks))]
   (= [2 3] (sort counts))))

(defn two-pairs? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (if (contains? (into #{} counts) 4) true ; handle case of 4
      (= 2 (get (frequencies counts) 2)))))

;; not working
(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        low-ace-sorted-ranks (sort (replace {14 1} ranks)) 
        min-rank (first ranks)
        min-sorted-rank (first low-ace-sorted-ranks)]
    (or (= sorted-ranks (range min-rank (+ 5 min-rank)))
        (= low-ace-sorted-ranks 
           (range min-sorted-rank (+ 5 min-sorted-rank))))))

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
