(ns p-p-p-pokerface)

(def face-map {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[value _] card]
    (if (Character/isDigit value)
          (Integer/valueOf (str value))
          (face-map value))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn pair? [hand]
  (contains? (set (vals (frequencies(map rank hand))))2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies(map rank hand))))3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies(map rank hand))))4))

(defn flush? [hand]
  (= 1 (count (distinct(map suit hand)))))


(defn full-house? [hand]
  (and (pair? hand)  (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  ( = 2 (count (filter #(= % 2) (vals (frequencies(map rank hand)))))))


(defn straight? [hand]
  (let [sorted (sort (map rank hand))]
    (if (=  sorted  (range (first sorted) (inc (last sorted))))
      true
      (if (some #(= 14 %) sorted)
        (straight? (map #(apply str(replace {\A \1} %)) hand))
        false))))


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  nil)
