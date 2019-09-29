(ns p-p-p-pokerface)

(def ranks {\T 10
            \J 11
            \Q 12
            \K 13
            \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get ranks r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn n-of-a-kind [n hand]
  (let [ranks (map rank hand)
        freqs (set (vals (frequencies ranks)))]
    (and (== (count freqs) 2)
         (contains? freqs n)
         (contains? freqs 1))))

(defn pair? [hand]
  (n-of-a-kind 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (set (vals (frequencies suits)))]
    (== (count freqs) 1)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= (sort freqs) '(2 3))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= (sort freqs) '(1 2 2))))

(defn straight? [hand]
  (let [ranks (filter (fn [rank] (not (== rank 14))) (map rank hand))
        lowest (apply min ranks)
        reduced (map (fn [rank] (- rank lowest)) ranks)
        reduced-sorted (sort reduced)]
    (or (= reduced-sorted '(0 1 2 3 4))
        (= reduced-sorted '(0 1 2 3)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        values (map
                (fn [checker]
                      (if ((first checker) hand)
                        (second checker)
                        0))
                checkers)]
    (apply max values)))

