(ns p-p-p-pokerface)

(defn suit [card]
  (let[[_ s] card]
    (str s)))
(defn rank [card]
   (let[[chr _] card]
     (if (Character/isDigit chr) (Integer/valueOf (str chr))
       ({\T 10,\J 11,\Q 12,\K 13, \A 14} chr))
  ))

(defn suit [card]
  (let[[_ s] card]
    (str s)))

(defn pair? [hand]
  (> (apply max(vals(frequencies(map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max(vals(frequencies(map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (== (apply max(vals(frequencies(map rank hand)))) 4))

(defn flush? [hand]
  (== (count(keys(frequencies(map suit hand)))) 1))

(defn full-house? [hand]
  (= [2 3] (sort(vals(frequencies(map rank hand))))))

(defn two-pairs? [hand]
  (let[help (fn [hand] (sort(vals(frequencies(map rank hand)))))]
    (or (= [1 4] (help hand)) (= [1 2 2] (help hand)))))


(defn straight? [hand]
  (let [[min-value :as sorted] (sort (map second hand))]
    (= sorted
      (take 5
        (iterate inc min-value)))))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
