(ns p-p-p-pokerface)


(defn suit [card]
  (let [[_ snd] card] (str snd)))

(defn rank [card]
  (let [[frs _] card]
    (cond
      (Character/isDigit frs) (Integer/valueOf (str frs))
      (= (str frs) "T") 10
      (= (str frs) "A") 14
      (= (str frs) "J") 11
      (= (str frs) "Q") 12
      (= (str frs) "K") 13)))

(defn pair? [hand]
  (not
    (empty?
      (filter
        (fn [x] (< 1 (second x)))
        (frequencies
          (map (fn [x] (rank x)) hand))))))

(defn three-of-a-kind? [hand]
  (not
    (empty?
      (filter
        (fn [x] (< 2 (second x)))
        (frequencies
          (map (fn [x] (rank x)) hand))))))

(defn four-of-a-kind? [hand]
  (not
    (empty?
      (filter
        (fn [x] (< 3 (second x)))
        (frequencies
          (map (fn [x] (rank x)) hand))))))

(defn flush? [hand]
  (not
    (empty?
      (filter
        (fn [x] (< 4 (second x)))
        (frequencies
          (map (fn [x] (suit x)) hand))))))

(defn full-house? [hand]
  (empty?
    (filter
      (fn [x] (= 1 (second x)))
      (frequencies
        (map (fn [x] (rank x)) hand)))))

(defn two-pairs? [hand]
  (or(= 2
    (count(filter
      (fn [x] (= 2 (second x)))
      (frequencies
        (map (fn [x] (rank x)) hand))))) (four-of-a-kind? hand)))

(defn has-ace? [hand]
  (not
    (empty?
      (filter
        (fn [x] (= 14 (first x)))
        (frequencies
          (map (fn [x] (rank x)) hand))))))

(defn straight? [hand]
  (let [min (apply min (map (fn [x] (rank x)) hand))
        maxi (apply max (map (fn [x] (rank x)) hand))
        ranks (sort(concat (map (fn [x] (rank x)) hand)))]
  (cond
      (and (= (range min (+ maxi 1)) ranks) (= 4 (- maxi min))) true
      (and(has-ace? hand) (= (range 1 (+ (apply max (replace {14 1} ranks)) 1) (sort(replace {14 1} ranks)))) (= 4 (- (apply max (replace {14 1} ranks)) 1))) true
      :else false)))

(defn all-same-color? [hand]
  (not
    (empty?
      (filter
        (fn [x] (= 5 (second x)))
        (frequencies
          (map (fn [x] (suit x)) hand))))))

(defn straight-flush? [hand]
  (if(not(straight? hand)) false (if(all-same-color? hand) true false)))

(defn high-card? [hand] true)


(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (apply max (map (fn [x] (if(= true ((first x) hand)) (second x) 0)) checkers))))

