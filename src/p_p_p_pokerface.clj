(ns p-p-p-pokerface)


(defn rank [card]
(def replacements {\A 14, \Q 12, \K 13, \J 11, \T 10})
    (let [[fird _] card]
    (if (Character/isDigit fird) (Integer/valueOf (str fird))  (Integer/valueOf (replacements fird)))))


(defn suit [card]
  (let [[_ scnd] card]
  (str scnd)))

(defn pair? [hand]
 (let [max
       (apply max (vals (frequencies (map rank hand))))]
   (if (= max 2) true false)))

(defn three-of-a-kind? [hand]
 (let [max
       (apply max (vals (frequencies (map rank hand))))]
   (if (= max 3) true false)))


(defn four-of-a-kind? [hand]
  (let [max
       (apply max (vals (frequencies (map rank hand))))]
   (if (= max 4) true false)))


(defn flush? [hand]
  (let [vari
       (vals (frequencies (map suit hand)))]
   (if (= (count vari) 1) true false)))


(defn full-house? [hand]
 (defn underpair? [hand]
 (let [min
       (apply min (vals (frequencies (map rank hand))))]
   (if (= min 2) true false)))
  (if (underpair? hand) (if (three-of-a-kind? hand) true false) false))


(defn two-pairs? [hand]
  (let [counts  (vals (frequencies (map rank hand)))]
    (set counts)
   (if (< (count counts) 4) (if (three-of-a-kind? hand) false true) false)))


(defn straight? [hand]
  (let [ranks (sort (set(map rank hand)))
    counts(count (vals (frequencies ranks)))
        max (apply max ranks)
         min (apply min ranks)]
     (if (= counts 5 )(if (= 4 (- max min)) true (if (contains? (set(map rank hand)) 14) true false)) false)))


(defn straight-flush? [hand]
 (if (straight? hand) (if (flush? hand) true false) false))

(defn value [hand]
  (if (straight-flush? hand) 8 (if (four-of-a-kind? hand) 7 (if (full-house? hand) 6 (if (flush? hand) 5 (if (straight? hand) 4
  (if(three-of-a-kind? hand) 3 (if(two-pairs? hand) 2 (if(pair? hand) 1 0)))))))))
