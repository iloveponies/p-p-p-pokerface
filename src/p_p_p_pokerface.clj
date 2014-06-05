(ns p-p-p-pokerface)

(def rank-map {\A "14" 
               \K "13"
               \Q "12",
               \J "11",
               \T "10"})

(defn rank [card]
  (let [[rank-char _] card]
    (Integer/valueOf (if (Character/isDigit rank-char)
                       (str rank-char)
                       (get rank-map rank-char)))))

(defn suit [card]
  (let [[_ suit-char] card]
    (str suit-char)))

(defn kind-counts [hand]
  (let [ranks (map rank hand)]
    (->> ranks
         (frequencies ,,)
         (vals ,,)
         (sort ,,))))

(defn max-kind [hand]
  (apply max (kind-counts hand)))

(defn pair? [hand]
  (= 2 (max-kind hand)))

(defn three-of-a-kind? [hand]
  (= 3 (max-kind hand)))

(defn four-of-a-kind? [hand]
  (= 4 (max-kind hand)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= 1 (count (set suits)))))

(defn full-house? [hand]
  (= [2 3] (kind-counts hand)))

(defn two-pairs? [hand]
  (or (= [1 2 2] (kind-counts hand))
      (= [1 4] (kind-counts hand))))

(defn ranks [hand]
  (sort (map rank hand)))

(defn increase-by-one? [num-seq]
  (loop [pairs (partition 2 1 (sort num-seq))]
    (cond (empty? pairs)
          ,,true
          (= -1 (apply - (first pairs)))
          ,,(recur (next pairs))
          :t
          ,,false)))

(defn straight? [hand]
  (let [hand-ranks (ranks hand)]
    (or (increase-by-one? hand-ranks)
        (increase-by-one? (replace {14 1} hand-ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)


(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (->> checkers
         (filter (fn [[checker _]]  (checker hand)) ,,)
         (map second ,,)
         (apply max ,,))))


(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

