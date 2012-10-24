(ns p-p-p-pokerface)

(def rankmap {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[r _] card]
    (cond
     (Character/isDigit r) (Integer/valueOf (str r))
     :else (get rankmap r))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [shand (map rank hand)
        freqs (sort (vals (frequencies shand)))]
    (and 
     (= 2 (count freqs))
     (= 2 (first freqs))
     (= 3 (second freqs)))))

(defn two-pairs? [hand]
  (let [shand (map rank hand)
        freqs (filter 
               (fn [x] (<= 2 x)) 
               (vals (frequencies shand)))]
     (or (<= 2 (count freqs)) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [rhand (sort (map rank hand))
        lace (sort (replace {14 1} rhand))
        buildr (fn [svec] (range (first svec) (+ 1 (last svec))))]
    (or (= lace (buildr lace)) (= rhand (buildr rhand)))))

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