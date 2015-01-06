(ns p-p-p-pokerface)



(defn suit [[_ snd]]
  (str snd))

(defn rank [[fst _]]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (let
      [replacements {\T 10
                     \J 11
                     \Q 12
                     \K 13
                     \A 14}] (get replacements fst))))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn diff [s] (map - (rest s) s))

(defn flush? [hand]
    (apply = (map suit hand)))

(defn full-house? [hand]
  (let [
        freqs (set (vals (frequencies (map rank hand))))
        ]
    (and (contains? freqs 2) (contains? freqs 3)))
  )

(defn two-pairs? [hand]
  (or
   (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2))
   (= 1 (get (frequencies (vals (frequencies (map rank hand)))) 4))))

(defn straight? [hand]
  (let [
        check (fn [suits ranks]
               (if (>= (count (set suits)) 1)
    (apply = (conj (diff (sort ranks)) 1))
    false
    ))] (or
         (check (map suit hand) (map rank hand))
         (check (map suit hand) (replace {14 1} (map rank hand)))

         )
  ))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map (fn [x] (get x 1)) (
     filter (fn [x] ((get x 0) hand)) checkers)
    ))))
