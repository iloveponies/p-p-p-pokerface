(ns p-p-p-pokerface)


;; done
(def picturecards {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let[[rnk _] card]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (picturecards rnk))))

;; done
(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-freqs [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

;; done
(defn pair? [hand]
    (let [freqs (rank-freqs hand)]
      (< 1 (apply max freqs))))

;; done
(defn three-of-a-kind? [hand]
  (let [freqs (rank-freqs hand)]
      (< 2 (apply max freqs))))

;; done
(defn four-of-a-kind? [hand]
  (let [freqs (rank-freqs hand)]
      (< 3 (apply max freqs))))

;; done
(defn flush? [hand]
  (let [suits (map suit hand)]
    (let [suit-counts (vals (frequencies suits))]
      (== 5 (apply max suit-counts)))))

;; done
(defn full-house? [hand]
  (let [freqs (sort (rank-freqs hand))]
    (= [2 3] freqs)))


(defn two-pairs? [hand]
  (let [freqs (sort (rank-freqs hand))]
    (or (= [1 2 2] freqs)
        (four-of-a-kind? hand))))

(defn is-straight [ranks]
  (let [minrank (apply min ranks)]
    (let [straight (range minrank (+ minrank 5))]
      (= (sort ranks) straight))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (let [low-ranks (replace {14 1} ranks)]
      (or (is-straight ranks)
          (is-straight low-ranks)))))


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)


(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    (high-card? hand) 0))


