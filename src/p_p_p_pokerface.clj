(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-of-a-kind? [hand n]
  (if (= n (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [ranks (map suit hand)]
    (if (= 5 (nth (vals (frequencies ranks)) 0))
      true
      false)))

(defn full-house? [hand]
  (if (= [2 3] (sort (vals (frequencies (map rank hand)))))
    true
    false))

(defn two-pairs? [hand]
  (if (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
    true
    false))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        lowest (nth sorted-hand 0)
        highest (nth sorted-hand 4)
        replaced-hand (sort (replace {1 14} sorted-hand))
        lowest-replaced (nth replaced-hand 0)
        highest-replaced (nth replaced-hand 4)
        replaced-hand2 (sort (replace {14 1} sorted-hand))
        lowest-replaced2 (nth replaced-hand2 0)
        highest-replaced2 (nth replaced-hand2 4)]
    (and (or (= highest (+ lowest 4))
             (= highest-replaced (+ lowest-replaced 4))
             (= highest-replaced2 (+ lowest-replaced2 4)))
         (= [1 1 1 1 1] (vals (frequencies (map rank hand)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  nil)
