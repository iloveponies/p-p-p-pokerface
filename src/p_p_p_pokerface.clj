(ns p-p-p-pokerface)


(defn
  suit
  [card]
    (let [[fst snd] card]
      (str snd)))


(defn
  rank
  [card]
  (let [[fst snd] card]
   (if
    (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))


(defn
  pair?
  [hand]
  (<= 2 (apply max
              (map second (frequencies
              (map rank hand))))))


(defn
  three-of-a-kind?
  [hand]
  (= 3 (apply max
              (map second (frequencies
              (map rank hand))))))

(defn
  four-of-a-kind?
  [hand]
   (= 4 (apply max
              (map second (frequencies
              (map rank hand))))))

(defn
  flush?
  [hand]
  (= 5 (apply max
              (map second (frequencies
              (map suit hand))))))


(defn
  full-house?
  [hand]
  (if (= [2 3] (sort (seq
         (vals
         (frequencies
           (map rank hand))))))
          true
          false))


(defn
  two-pairs?
  [hand]
  (<= 4 (apply *
               (vals
               (frequencies
                 (map rank hand))))))


(defn
  straight?
  [hand]
  (let [sh (sort (map rank hand))]
    (if
    (and (apply < sh) (== (+ 4 (apply min sh))
                          (apply max sh))) true
      (= sh (seq [2 3 4 5 14])))))

(defn
  straight-flush?
  [hand]
  (and (straight? hand) (flush? hand)))


(defn
  high-card?
  [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max
           (map second
                (filter (fn [x] ((first x) hand))
                        checkers)))))
