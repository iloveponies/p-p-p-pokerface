(ns p-p-p-pokerface)

(defn numb? [char]
  (Character/isDigit char))

(defn nrank [n]
  (Integer/valueOf (str n)))

(defn crank [c]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (replacements c))

(defn rank [card]
  (let [[snd] card]
  (cond
    (numb? snd) (nrank snd)
    :else (crank snd))))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (contains? (set (vals (frequencies (map rank hand)))) 2) (contains? (set (vals (frequencies (map rank hand)))) 3)))

(defn two-pairs? [hand]
  (or (= (sort (vals (frequencies (map rank hand)))) [1 2 2]) (four-of-a-kind? hand) (full-house? hand)))

(defn straight? [hand]
  (let [ranks (sort(map rank hand))]
    (if (= [2 3 4 5 14] ranks) true
    (= ranks (range (first ranks) (+ 1 (last ranks)))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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
    :else 0))
