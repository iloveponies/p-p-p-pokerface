(ns p-p-p-pokerface)

(def special-ranks {\T 10
                    \J 11
                    \Q 12
                    \K 13
                    \A 14})

(defn rank [card]
  (let [[first second] card]
    (if (Character/isDigit first)
      (Integer/parseInt (str first))
      (get special-ranks first))))

(defn ranks [hand]
  (map rank hand))

(defn suit [card]
  (let [[first second] card]
    (str second)))

(defn suits [hand]
  (map suit hand))

(defn n-of-a-kind
  "Checks whether the hand is EXACTLY an n-of-a-kind."
  [hand n]
  (= (apply max (vals (frequencies (ranks hand)))) n))

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (apply = (suits hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (get (frequencies (vals (frequencies (ranks hand)))) 2) 2)))

(defn straight? [hand]
  (let [s? (fn [hand-ranks]
             (let [sorted (sort hand-ranks)]
               (= sorted (range (first sorted) (+ (last sorted) 1)))))
        r (ranks hand)]
    (or (s? r)
        (s? (replace {14 1} r)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  nil)
