(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        arvot {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
       (Integer/valueOf (str rank))
       (get arvot rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freq (set (vals (frequencies ranks)))]
   (contains? freq 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (set (vals (frequencies ranks)))]
   (contains? freq 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (set (vals (frequencies ranks)))]
   (contains? freq 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        freq (set (vals (frequencies suits)))]
   (contains? freq 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freq (set (vals (frequencies ranks)))]
    (and (contains? freq 3) (contains? freq 2))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq (filterv (fn [value] (>= value 2)) (vals (frequencies ranks)))]
    (or (>= (count freq) 2) (= (get freq 0) 4) (= (get freq 1) 4))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        lowest (apply min ranks)
        highest (apply max ranks)
        sorted-ranks (if (and (= lowest 2) (= highest 14))
                        (sort (replace {14 1} ranks))
                        (sort ranks))
        check (if (and (= lowest 2) (= highest 14))
                 (range 1 6)
                 (range lowest (+ lowest 5)))]
    (= sorted-ranks check)))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[true 0]  [(pair? hand) 1]
                 [(two-pairs? hand) 2]  [(three-of-a-kind? hand) 3]
                 [(straight? hand) 4]   [(flush? hand) 5]
                 [(full-house? hand) 6] [(four-of-a-kind? hand) 7]
                 [(straight-flush? hand) 8]}
        check (fn [vector] (first vector))
        passed (filter check checkers)
        points (mapv second passed)
        highest-points (apply max points)]
    highest-points))

