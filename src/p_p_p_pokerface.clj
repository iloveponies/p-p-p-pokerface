(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (Integer/valueOf
     (str
      (if (Character/isDigit r)
        r
        (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
          (replacements r)))))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [frekvenssit (vals (frequencies (map rank hand)))]
    (= [2 3] (sort frekvenssit))))

; fix this when no pair present
(defn two-pairs? [hand]
  (let [frekvenssit (vals (frequencies (map rank hand)))]
    (if (four-of-a-kind? hand)
      true
      (= [1 2 2] (sort frekvenssit)))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
