(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card]
    (let [maps {"1" 1 "2" 2 "3" 3
                "4" 4 "5" 5 "6" 6
                "7" 7 "8" 8 "9" 9
                "T" 10 "J" 11 "Q" 12
                "K" 13 "A" 14}]
      (get maps (str rnk)))))

(defn suit [card]
  (let [[_ suitt] card] (str suitt)))

(defn frequency [hand]
  (if (empty? hand)
    {}
    (let [card (first hand)
          partial-freq (frequency (rest hand))
          inc-or-one (fnil inc 0)
          ]
      (assoc partial-freq (rank card)
             (inc-or-one (get partial-freq (rank card)))))))

(defn pairs [hand]
  (filter (fn [x] ( =  (last x) 2))
        (frequency hand)))

(defn pair? [hand]
  (pos? (count (pairs hand))))

(defn three-of-a-kind? [hand]
  (boolean
   (some (fn [x] ( =  (last x) 3))
         (frequency hand))))

(defn four-of-a-kind? [hand]
  (boolean
   (some (fn [x] ( =  (last x) 4))
         (frequency hand))))

(defn flush? [hand]
  (let [kind (suit (first hand))]
   (every? (fn [x] ( = (suit x) kind)) hand)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  ( = 2 (count (pairs hand))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
