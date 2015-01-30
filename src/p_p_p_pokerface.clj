(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank->int [r]
  (Integer/valueOf (str r)))

(defn rank [card]
  (let [r (get card 0)]
    (if (Character/isDigit r) (rank->int r) (rank->int (replacements r)))))

(defn suit [card]
  (str (get card 1)))

(defn elem [values value]
  (boolean (some #{value} values)))

(defn pair? [hand]
  (elem (vals (frequencies (map rank hand))) 2))

(defn three-of-a-kind? [hand]
  (elem (vals (frequencies (map rank hand))) 3))

(defn four-of-a-kind? [hand]
  (elem (vals (frequencies (map rank hand))) 4))

(defn flush? [hand]
  (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= (count (filter #{2} (vals (frequencies (map rank hand))))) 2))

(defn straight? [hand]
  (let [r (sort (map rank hand))
	s (apply min r)]
    (or (= r (range s (+ s 5)))
        (= r (seq [2 3 4 5 14])))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (if (straight-flush? hand) 8
    (if (four-of-a-kind? hand) 7
      (if (full-house? hand) 6
        (if (flush? hand) 5
          (if (straight? hand) 4
            (if (three-of-a-kind? hand) 3
              (if (two-pairs? hand) 2
                (if (pair? hand) 1 0)))))))))
