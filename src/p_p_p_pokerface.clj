(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (.indexOf [nil nil \2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A] rank))

(defn suit [[_ suit]] (str suit))

(defn same-values [values]
  (apply max (vals (frequencies values))))

(defn pair? [hand]
  (= (same-values (map rank hand)) 2))

(defn three-of-a-kind? [hand]
  (= (same-values (map rank hand)) 3))

(defn four-of-a-kind? [hand]
  (= (same-values (map rank hand)) 4))

(defn flush? [hand]
  (= (same-values (map suit hand)) 5))

(defn full-house? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (and (= 3 (apply max ranks)) (= 2 (apply min ranks)))))

(defn two-pairs? [hand]
  (let [ranks (sort > (vals (frequencies (map rank hand))))]
      (and (= 2 (first ranks) (second ranks)))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (and
      (= (count ranks) (count (set ranks)))
      (or
        (= 4 (- (last ranks) (first ranks)))
        (= [2 3 4 5 14] ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(def hand-fns [
    high-card?
    pair?
    two-pairs?
    three-of-a-kind?
    straight?
    flush?
    full-house?
    four-of-a-kind?
    straight-flush?
  ])

(defn value [hand]
  (apply max (map-indexed (fn [i hand-fn]
        (if (hand-fn hand) i 0)) hand-fns)))
