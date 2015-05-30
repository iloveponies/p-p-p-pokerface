(ns p-p-p-pokerface)

(declare n-of-a-kind?)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[first _] card]
    (cond
     (Character/isDigit first) (Integer/valueOf (str first))
     :else (replacements first))))

(defn suit [card]
  (let [[_ second] card]
    (str second)))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn n-of-a-kind? [n hand]
  (== n (apply max
               (vals (frequencies
                      (map rank hand))))))

(defn flush? [hand]
  (== 1 (count (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (range 2 4) (sort
             (vals (frequencies
                    (map rank hand))))))

(defn two-pairs? [hand]
  (or
   (four-of-a-kind? hand)
   (== 2 (count
          (filter (fn [value] (== 2 value))
                  (vals (frequencies
                         (map rank hand))))))))

(defn straight? [hand]
  (let [normalized-high-hand (sort  (map rank hand))
        normalized-low-hand (sort (replace {14 1} (map rank hand)))]
    (or
     (= (range (first normalized-high-hand) (+ 5 (first normalized-high-hand))) normalized-high-hand)
     (= (range (first normalized-low-hand) (+ 5 (first normalized-low-hand))) normalized-low-hand))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        matching-hands (filter (fn [matcher] (= true ((first matcher) hand))) checkers)
        hand-values (map second matching-hands)]
    (apply max hand-values)))
