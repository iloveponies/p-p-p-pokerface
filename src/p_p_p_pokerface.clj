(ns p-p-p-pokerface)

(def replacements {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8
                   \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14})

(def straights
  (conj (set (map (fn [x] (range x (+ x 5))) (range 2 11)))
        '(2 3 4 5 14)))

(defn rank [card]
  (get replacements (first card)))

(defn suit [card]
  (str (last card)))

(defn freqs [func hand]
  (vals (frequencies (map func hand))))

(defn freqs->set [func hand]
  (set (freqs func hand)))

(defn pair? [hand]
  (contains? (freqs->set rank hand) 2))

(defn three-of-a-kind? [hand]
  (contains? (freqs->set rank hand) 3))

(defn four-of-a-kind? [hand]
  (contains? (freqs->set rank hand) 4))

(defn flush? [hand]
  (= 1 (count (freqs suit hand))))

(defn full-house? [hand]
  (let [rank-freqs (freqs->set rank hand)]
    (and (contains? rank-freqs 2)
         (contains? rank-freqs 3))))

(defn two-pairs? [hand]
  (= 2 (count (filter (fn [x] (= 2 x)) (freqs rank hand)))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))]
    (contains? straights sorted-ranks)))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
