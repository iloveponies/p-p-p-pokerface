(ns p-p-p-pokerface)

(def toppkortene {\T 10
                  \J 11
                  \Q 12
                  \K 13
                  \A 14})

(defn rank [card]
  (let [[first _] card]
    (if (Character/isDigit first)
      (Integer/valueOf (str first))
      (get toppkortene first))))

(defn suit [card]
  (let [[_ second] card]
    (str second)))

(defn max-frekvens [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (= 2 (max-frekvens hand)))

(defn three-of-a-kind? [hand]
  (= 3 (max-frekvens hand)))

(defn four-of-a-kind? [hand]
  (= 4 (max-frekvens hand)))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (range 2 4) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or
    (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2))
    (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [handverdier (map rank hand)
        handverdier-b (replace {14 1} handverdier)]
    (and
      (= 1 (max-frekvens hand))
      (or
        (= 4 (- (min handverdier) (max handverdier)))
        (= 4 (- (min handverdier-b) (max handverdier-b)))))))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)