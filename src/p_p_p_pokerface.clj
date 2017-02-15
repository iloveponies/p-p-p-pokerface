(ns p-p-p-pokerface)


(defn
  suit
  [card]
  (let [[_ x] card]
      (str x)))

(defn
  rank
  [card]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [x _] card]
          (if (Character/isDigit x)
            (Integer/valueOf (str x))
            (replacements x))))

(defn
  pair?
  [hand]
      (let [x (map rank hand)
            xx (vals (frequencies x))
            xxx (apply max xx)]
              (= xxx 2)))

(defn
  three-of-a-kind?
  [hand]
  (let [x (map rank hand)
        xx (vals (frequencies x))
        xxx (apply max xx)]
          (= xxx 3)))

(defn
  four-of-a-kind?
  [hand]
    (let [x (map rank hand)
            xx (vals (frequencies x))
            xxx (apply max xx)]
              (= xxx 4)))

(defn
  flush?
  [hand]
       (let [x (map suit hand)
             xx (vals (frequencies x))
             xxx (first xx)]
               (= xxx 5)))


(defn
  full-house?
  [hand]
    (let [x (map rank hand)
          xx (vals (frequencies x))]
            (= [2 3] (sort xx))))


(defn
  two-pairs?
  [hand]
  (let [x (map rank hand)
    xx (vals (frequencies x))]
      (= [1 2 2] (sort xx))))


(defn
  straight?
  [hand]
    (let [x (map rank hand)
          xx (sort x)
          xxx (apply max xx)
          xxxx (apply min xx)
          xxxxx (range xxxx (+ 1 xxx))
          xxxxxx (range 1 6)]
            (if (= (count xxxxx) 13)
              true
              (= xxxxx xx))))


(defn
  straight-flush?
  [hand]
  (let [x (flush? hand)
        xx (straight? hand)]
          (and x xx)))

(defn
  value
  [hand]
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
