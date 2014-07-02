(ns p-p-p-pokerface)

(defn rank [card]
  (let
    [[fst _] card
     replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
     ]
    (cond
     (Character/isDigit fst) (Integer/valueOf (str fst))
     :else (replacements fst))))

(defn suit [card]
  (let
    [[_ snd] card]
    (str snd)))

(defn pair? [hand]

  (let
    [rankSet (vals (frequencies (map rank hand))) ; get a set containing frequencies of ranks within the hand
     hasPair (not (empty? (filter (fn [rank] (= rank 2)) rankSet))) ; filter set to frequencies of 2
     ]

    hasPair))

(defn three-of-a-kind? [hand]
    (let
    [rankSet (vals (frequencies (map rank hand))) ; get a set containing frequencies of ranks within the hand
     hasTrips (not (empty? (filter (fn [rank] (= rank 3)) rankSet))) ; filter set to frequencies of 3
     ]

    hasTrips))

(defn four-of-a-kind? [hand]
    (let
    [rankSet (vals (frequencies (map rank hand))) ; get a set containing frequencies of ranks within the hand
     hasQuads (not (empty? (filter (fn [rank] (= rank 4)) rankSet))) ; filter set to frequencies of 4
     ]

    hasQuads))

(defn flush? [hand]
    (let
    [suitSet (vals (frequencies (map suit hand))) ; get a set containing frequencies of suits within the hand
     hasFlush (not (empty? (filter (fn [suit] (= suit 5)) suitSet))) ; filter set to frequencies of 5 aka all the same suit
     ]

    hasFlush))

(defn full-house? [hand]
  (= true (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (let
    [rankSet (vals (frequencies (map rank hand))) ; get a set containing frequencies of ranks within the hand
     hasTwoPairs (= 2 (get (frequencies rankSet) 2)) ; filter set to frequencies of 2
     ]

    hasTwoPairs))

(defn straight? [hand]
  (let
    [
     rankSetHighAce (sort (map rank hand))
     rankSetLowAce (sort (replace {14 1} (map rank hand)))
     ]

    (cond
     (= rankSetLowAce (range (first rankSetLowAce) (+ (first rankSetLowAce) 5))) true
     (= rankSetHighAce (range (first rankSetHighAce) (+ (first rankSetHighAce) 5))) true
     :else false

     )
    ))

(defn straight-flush? [hand]
  (= true (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}

        applyChecker (fn [checker]
                       (if (= true ((first checker) hand)) (second checker) 0))

        ]
    (apply max (map applyChecker checkers))
    ))
