(ns p-p-p-pokerface)

;; Translation for face cards
(def rank-map {\T 10
               \J 11
               \Q 12
               \K 13
               \A 14})

;; Exercise 2
;; Card -> Rank
(defn rank [[r _]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (get rank-map r)))

;; Exercise 1
;; Card -> Suit
(defn suit [[_ s]]
  (str s))

;; Helper function
;; Hand, Integer[2 4] -> Boolean
;; True if there is a at least one grouping of input size in the hand
(defn groups [hand size]
  (if (<= 2 size 4)
    (let [result (some #(= % size) (vals (frequencies (map rank hand))))]
      (if (nil? result)
        false
        result))))

;; Exercise 3
;; Hand -> Boolean
;; True if Hand contains at least a pair
(defn pair? [hand]
  (groups hand 2))

;; Exercise 3
;; Hand -> Boolean
;; True if Hand contains three-of-a-kind
(defn three-of-a-kind? [hand]
  (groups hand 3))

;; Exercise 3
;; Hand -> Boolean
;; True if Hand contains four-of-a-kind
(defn four-of-a-kind? [hand]
  (groups hand 4))

(defn flush? [hand]
  (= (first (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and
   (three-of-a-kind? hand)
   (pair? hand)))


(defn two-pairs? [hand]
  (let [same (count (filter #(or (= % 2)) (vals (frequencies (map rank hand)))))]
    (or (= same 2)
        (groups hand 4))))

(defn ace-low-hand [hand]
  (let [hand-sorted-by-rank (sort (map rank hand))]
    (if (contains? (set hand-sorted-by-rank) 14)
      (sort (replace {14 1} hand-sorted-by-rank))
      hand-sorted-by-rank)))

(defn straight? [hand]
  (let [sorted-list-low (ace-low-hand hand)
        sorted-list-high (sort (map rank hand))
        size (count hand)
        base-low (first sorted-list-low)
        base-high (first sorted-list-high)
        gen-list-low (range base-low (+ base-low size))
        gen-list-high (range base-high (+ base-high size))]
    (or
       (= sorted-list-low gen-list-low)
       (= sorted-list-high gen-list-high))))

(defn straight-flush? [hand]
  (and
   (flush? hand)
   (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check-fn (fn [item]
                  (let [checking-fn (first item)
                        hand-value (second item)]
                    (if (checking-fn hand)
                      hand-value
                      0)))]
    (apply max (map check-fn checkers))))












