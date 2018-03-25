(ns p-p-p-pokerface)

(defn rank [card]
  (let [char-map {\T 10, \J 11, \Q 12, \K 13, \A 14}
        character->digit (fn [char] (get char-map char))
        [first _] card]
    (if (Character/isDigit first)
      (Integer/valueOf (str first))
      (character->digit first))))

(defn suit [card]
  (let [[_ second] card]
    (str second)))

(defn pair? [hand]
  (== (apply
       max
       (vals
         (frequencies
           (map rank hand))))
     2))

(defn three-of-a-kind? [hand]
  (== (apply
       max
       (vals
         (frequencies
           (map rank hand))))
     3))

(defn four-of-a-kind? [hand]
  (== (apply
       max
       (vals
         (frequencies
           (map rank hand))))
     4))

(defn flush? [hand]
      (apply = (map suit hand)))

(defn full-house? [hand]
      (let [pairs (vals (frequencies (map rank hand)))]
           (or (= [2 3] pairs) (= [3 2] pairs))))

(defn two-pairs? [hand]
      (let [pairs (vals (frequencies (map rank hand)))]
           (= [1 2 2] (sort pairs))))

(defn straight? [hand]
  (let [a->1 (replace {14 1} (map rank hand))
        a->14 (map rank hand)
        maximum (fn [arg] (apply max arg))
        minimum (fn [arg] (apply min arg))
        monotonic (fn [seq] (apply < (sort seq)))
        a-1-diff (- (maximum a->1) (minimum a->1))
        a-14-diff (- (maximum a->14) (minimum a->14))]
     (and
       (or
         (monotonic a->1)
         (monotonic a->14))
       (or
         (= 4 a-1-diff)
         (= 4 a-14-diff)
         ))))


(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))

(defn value [hand]
  (cond
    (straight-flush?  hand) 8
    (four-of-a-kind?  hand) 7
    (full-house?      hand) 6
    (flush?           hand) 5
    (straight?        hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs?       hand) 2
    (pair?            hand) 1
    :else             0))
