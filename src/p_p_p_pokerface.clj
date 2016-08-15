(ns p-p-p-pokerface)

(defn rank [card]
  (let [valuemap {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [rank _] card]
     (if (Character/isDigit rank)
       (Integer/valueOf (str rank))
       (get valuemap rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [parit (fn [x] (<= 2 x))]
   (not (empty?
         (filter parit
                 (vals (frequencies (map rank hand))))))))

(defn three-of-a-kind? [hand]
  (let [parit (fn [x] (<= 3 x))]
   (not (empty?
         (filter parit
                 (vals (frequencies (map rank hand))))))))

(defn four-of-a-kind? [hand]
  (let [parit (fn [x] (<= 4 x))]
   (not (empty?
         (filter parit
                 (vals (frequencies (map rank hand))))))))

(defn flush? [hand]
  (== 5 (first (vals (frequencies
                      (map suit hand))))))

(defn full-house? [hand]
  (= (seq [2 3])
     (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [arvot (sort (vals (frequencies (map rank hand))))]
    (or (= (seq [1 2 2]) arvot)
        (= (seq [2 3]) arvot)
        (= (seq [1 4]) arvot))))

(defn straight? [hand]
  (let [ordered-hand (sort (map rank hand))
        lowest (first ordered-hand)
        highest (last ordered-hand)]
    (or (= ordered-hand (range lowest (+ 1 highest)))
        (= ordered-hand (seq [2 3 4 5 14])))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
