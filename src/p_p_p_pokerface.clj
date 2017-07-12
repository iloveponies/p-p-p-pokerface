(ns p-p-p-pokerface)

(defn rank [card]
  (let [shorts {\T 10
                \J 11
                \Q 12
                \K 13
                \A 14}
        [r _] card]
    (if (Character/isDigit r)
        (Integer/valueOf (str r))
        (get shorts r))))


(defn suit [card]
  (let [[_ s] card]
    (str s)))


(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        counts (vals freqs)]
    (not (empty? (filter (fn [x] (> x 1)) counts)))))


(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        counts (vals freqs)]
    (not (empty? (filter (fn [x] (> x 2)) counts)))))


(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        counts (vals freqs)]
    (not (empty? (filter (fn [x] (> x 3)) counts)))))


(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))


(defn full-house? [hand]
  (let [freqs (frequencies (map rank hand))
        ordfreqs (sort (vals freqs))]
    (= ordfreqs [2 3])))


(defn two-pairs? [hand]
  (let [freqs (frequencies (map rank hand))
        ordfreqs (sort (vals freqs))]
    (or (= ordfreqs [1 4])
        (= ordfreqs [1 2 2]))))


(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ace-low-ranks (sort (map (fn [x] (if (= x 14) 1 x)) ranks))
        minrank (fn [x] (apply min x))
        normalize (fn [x]
                    (let [m (minrank x)]
                      (map (fn [y] (- y m)) x)))]

    (or (= [0 1 2 3 4] (normalize ranks))
        (= [0 1 2 3 4] (normalize ace-low-ranks)))))


(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))


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
    :else 0
    ))


