(ns p-p-p-pokerface)

(def char->rank {"T" 10, "J" 11, "Q" 12, "K" 13, "A" 14})
(def char->rank-a {"T" 10, "J" 11, "Q" 12, "K" 13, "A" 1})

(defn rank-function [card f]
  (let [[r _] card]
   (if (Character/isDigit r)
     (Integer/valueOf (str r))
     (f (str r)))))

(defn rank [card]
  (rank-function card char->rank)
  )

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn same-cards? [hand n f]
  (not (empty?
        (filter
         #(= 0 (mod % n))
         (vals
          (frequencies
           (map f hand)))))))

(defn pair? [hand]
  (same-cards? hand 2 rank))


(defn three-of-a-kind? [hand]
  (same-cards? hand 3 rank))

(defn four-of-a-kind? [hand]
  (same-cards? hand 4 rank))

(defn flush? [hand]
  (same-cards? hand 5 suit))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))


(defn two-pairs? [hand]
  (= 4 (apply + (filter #(= 0 (mod % 2)) (vals
    (frequencies
      (map rank hand)))))))

(defn same-seq? [hand f]
  (let [rg (map f hand)]
    (= (sort rg) (range (apply min rg) (+ (apply max rg) 1))))
  )

(defn straight? [hand]
  (or (same-seq? hand rank) (same-seq? hand #(rank-function % char->rank-a)))
  )

(defn high-card? [hand]
  true)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map #(let [[f points] %]
            (if (f hand)
              points
              0
            )) checkers))))

