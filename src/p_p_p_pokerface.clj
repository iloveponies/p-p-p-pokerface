(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card]
    (cond
	  (Character/isDigit rank) (Integer/valueOf (str rank))
	  (= rank \T) 10
	  (= rank \J) 11
	  (= rank \Q) 12
	  (= rank \K) 13
	  (= rank \A) 14)))
	  
(defn ranks [hand]
  (map rank hand))
  
(defn ranks-frequencies [hand]
  (frequencies (ranks hand)))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))
	
(defn suits [hand]
  (map suit hand))
  
(defn high-card? [hand]
  true)

(defn pair? [hand]
  (contains? (set (vals (ranks-frequencies hand))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (ranks-frequencies hand))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (ranks-frequencies hand))) 4))

(defn flush? [hand]
  (== (count (set (suits hand))) 1))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
    (four-of-a-kind? hand)
    (and
      (== (count (vals (ranks-frequencies hand))) 3)
	  (not (three-of-a-kind? hand)))))

(defn straight? [hand]
  (let [this-contains? (fn [rank] (contains? (set hand) rank))
        this-contains-all? (fn [set-of-ranks] (not (contains? (set (apply this-contains? set-of-ranks)) false)))]
    (or
      (= (set (ranks hand)) #{2 3 4 5 14})
      (and
        (== (count (vals (ranks-frequencies hand))) 5)
        (== (+ (apply min (map rank hand)) 4) (apply max (ranks hand)))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
	(flush? hand)))
	
(def high-card {:function (fn [hand] (high-card? hand)) :value 0})
(def pair {:function (fn [hand] (pair? hand)) :value 1})
(def two-pairs {:function (fn [hand] (two-pairs? hand)) :value 2})
(def three-of-a-kind {:function (fn [hand] (three-of-a-kind? hand)) :value 3})
(def straight {:function (fn [hand] (straight? hand)) :value 4})
(def flushy {:function (fn [hand] (flush? hand)) :value 5})
(def full-house {:function (fn [hand] (full-house? hand)) :value 6})
(def four-of-a-kind {:function (fn [hand] (four-of-a-kind? hand)) :value 7})
(def straight-flush {:function (fn [hand] (straight-flush? hand)) :value 8})

(def types [high-card pair two-pairs three-of-a-kind straight flushy full-house four-of-a-kind straight-flush])

(defn hand-of-type? [hand type]
  (let [function (fn [hand] ((:function type) hand))]
    (function hand)))

(defn value [hand]
  (let [this-hand-of-type? (fn [type] (hand-of-type? hand type))]
    (apply max (map :value (filter this-hand-of-type? types)))))
