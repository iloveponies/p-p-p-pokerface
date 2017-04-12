(ns p-p-p-pokerface)

(defn rank [card]
  "Exercise 2"
  (def the-big-ones {\A 14, \K 13 \Q 12 \J 11 \T 10})
  (let [[rank _] card]
    (let [digit (Character/isDigit rank)]
      (if digit
        (Integer/valueOf (str rank))                        ;convert char containing digit to str to integer
        (the-big-ones rank)))))


(defn suit [card]
  "Exercise 1"
  (let [[_ suit] card]
    (str suit)))


(defn contain? [val, collection]
  (boolean (some #(= val %) collection)))


(defn pair? [hand]
  "Exercise 3"
  (let [ranks (map rank hand)]
    (let [occurences (vals (frequencies ranks))]
      (contain? 2 occurences))))


(defn three-of-a-kind? [hand]
  "Exercise 4"
  (let [ranks (map rank hand)]
    (let [occurences (vals (frequencies ranks))]
      (contain? 3 occurences))))


(defn four-of-a-kind? [hand]
  "Exercise 5"
  (let [ranks (map rank hand)]
    (let [occurences (vals (frequencies ranks))]
      (contain? 4 occurences))))


(defn flush? [hand]
  "Exercise 6"
  (let [suits (map suit hand)]
    (let [occurences (vals (frequencies suits))]
      (contain? 5 occurences))))


(defn full-house? [hand]
  "Exercise 7"
  (let [ranks (map rank hand)]
    (let [occurences (vals (frequencies ranks))]
      (and (contain? 2 occurences) (contain? 3 occurences)))))


(defn two-pairs? [hand]
  "Exercise 8"
  (let [ranks (map rank hand)]
    (let [occurences (vals (frequencies ranks))]
      (= (count (filter #{2} occurences)) 2))))


(defn straight? [hand]
  "Exercise 9"
  nil)

(defn straight-flush? [hand]
  "Exercise 10"
  nil)

(defn value [hand]
  "Exercise 11"
  nil)
