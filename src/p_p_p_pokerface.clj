(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})


(defn rank [card]
  (let [[rank-char _] card]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (replacements rank-char))))

(defn suit [card]
  (let [[_ second-char] card]
    (str second-char)))

(defn hand->rank-frequencies [hand]
  (frequencies (map rank hand)))

(defn hand->suit-frequencies [hand]
  (frequencies (map suit hand)))

(defn pair? [hand]
  (< 1 (apply max (vals (hand->rank-frequencies hand)))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (hand->rank-frequencies hand)))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (hand->rank-frequencies hand)))))

(defn flush? [hand]
  (= 5 (apply max (vals (hand->suit-frequencies hand)))))

(defn full-house? [hand]
  (let [rank-freqs (vals (hand->rank-frequencies hand))]
    (or (= [2 3] rank-freqs) (= [3 2] rank-freqs))))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
