(ns p-p-p-pokerface)
;(load "things")

(def letters {\A 14, \T 10, \J 11, \Q 12, \K 13})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (letters r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(suit "2H")
(suit "2H")
(rank "2H")

(defn max-freq [hand com]
  (apply max (vals (frequencies (map com hand)))))

(defn pair? [hand]
  (if (= 2 (max-freq hand rank))
    true
    false))

(defn three-of-a-kind? [hand]
  (if (= 3 (max-freq hand rank))
    true
    false))

(defn four-of-a-kind? [hand]
  (if (= 4 (max-freq hand rank))
    true
    false))

(defn flush? [hand]
  (if (= 5  (max-freq hand suit))
    true
    false))

(defn full-house? [hand]
  (if (and
        (= 2 (apply min (vals (frequencies (map rank hand)))))
        (three-of-a-kind? hand))
    true
    false))

(defn two-pairs? [hand]
  (let [frst (vals (frequencies (map rank hand)))
        scnd (vals (frequencies frst))]
    (or
      (four-of-a-kind? hand)
      (and
        (= 2 (apply max frst))
        (= 2 (apply max scnd))))))

(defn straight? [hand]
  (let [tokenized (vec (replace letters (map rank hand)))
        start (apply min tokenized)]
    (or
      (= (range start (+ start 5)) (sort tokenized))
      (= (sort tokenized) [2 3 4 5 14]))))


(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]  true)

(defn value [hand]
  (let [check #{
                 [high-card? 0]
                 [pair? 1]
                 [two-pairs? 2]
                 [three-of-a-kind? 3]
                 [straight? 4]
                 [flush? 5]
                 [full-house? 6]
                 [four-of-a-kind? 7]
                 [straight-flush? 8]}
        checked (filter #((first %) hand) check)
        onlyval (map #(second %) checked)]
    (apply max onlyval)))
; ___ ;
