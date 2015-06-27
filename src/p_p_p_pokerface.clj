(ns p-p-p-pokerface)

(def values {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[frst] card]
    (if (Character/isDigit frst)
      (Integer/valueOf (str frst))
      (values frst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn contains-n? [n hand]
  (not
  (empty?
   (filter
    (fn [x] (>= x n))
    (vals (frequencies (map rank hand)))))))

(defn pair? [hand]
 (contains-n? 2 hand))

(defn three-of-a-kind? [hand]
  (contains-n? 3 hand))

(defn four-of-a-kind? [hand]
  (contains-n? 4 hand))

(defn flush? [hand]
  (=
   (first
    (vals
     (frequencies
      (map suit hand))))
    5 ))

(defn full-house? [hand]
  (let
    [[frst scnd]
     (sort (vals
            (frequencies
             (map rank hand))))]
    (and
     (= frst 2)
     (= scnd 3))))

(defn two-pairs? [hand]
  (let [
       [frst scnd thrd]
       (sort
        (vals
         (frequencies
          (map rank hand))))]
   (or
    (= frst 4)
    (and
    (= scnd 2)
    (= thrd 2)))))

(defn straight? [hand]
  (let [v
        (sort
           (map rank hand))
        [frst _ _ _ lst] v]
    (if (and (= frst 2) (= lst 14))
      (= (range 1 6) (sort (replace {14 1} v)))
      (= (range frst (+ frst 5)) v))))

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
