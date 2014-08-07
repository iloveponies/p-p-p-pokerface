(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r s] card
      face-values {\T 10
             \J 11
             \Q 12
             \K 13
             \A 14}]
    (if (Character/isDigit r) (Integer/parseInt (str r))
      (get face-values r))))

(defn suit [card]
  (let [[r s] card]
  (str s)))

(defn ranks [hand]
  (map rank hand))

(defn suits [hand]
  (map suit hand))

(defn rank-counts [hand]
  (vals (frequencies (ranks hand))))

(defn multiples-count [hand num]
  (count (filter #(= num %) (rank-counts hand))))

(defn multiples? [hand num]
  (> (multiples-count hand num) 0))

(defn pair? [hand]
  (multiples? hand 2))

(defn three-of-a-kind? [hand]
  (multiples? hand 3))

(defn four-of-a-kind? [hand]
  (multiples? hand 4))

(defn flush? [hand]
  (= 1 (count (set (suits hand)))))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
   (= (multiples-count hand 2) 2)
   (four-of-a-kind? hand)))

(defn straight? [hand]
   (let [ordered-hand (sort (ranks hand))
         lowest-card (apply min ordered-hand)
         ref-straight (range lowest-card (+ lowest-card 5))]
     (or (= ordered-hand ref-straight)
         (if (= (apply max ordered-hand) 14)
           (let [alt-hand (sort (replace {14 1} ordered-hand))]
             (= alt-hand (range 1 6)))
           false))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn value [hand]
  nil)
