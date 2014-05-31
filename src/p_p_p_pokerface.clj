(ns p-p-p-pokerface)

(defn rank [card]
  (let [[face _] card
        royalty {"T" 10 "J" 11 "Q" 12 "K" 13 "A" 14}]
    (cond
     (Character/isDigit face) (Integer/valueOf (str face))
     :else (get royalty (str face)))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (not
   (empty?
    (filter (fn [count]
              (> (second count) 1))
            (frequencies
             (map rank hand))))))

(defn three-of-a-kind? [hand]
  (not
   (empty?
    (filter (fn [count]
              (= (second count) 3))
            (frequencies
             (map rank hand))))))

(defn four-of-a-kind? [hand]
  (not
   (empty?
    (filter (fn [count]
              (= (second count) 4))
            (frequencies
             (map rank hand))))))

(defn flush? [hand]
  (= 1
     (count
      (frequencies
       (map suit hand)))))

(defn full-house? [hand]
  (let [frequency (mapv second
                        (frequencies (map rank hand)))]
    (= [2 3] (sort frequency))))

(defn two-pairs? [hand]
  (or
   (four-of-a-kind? hand)
   (= 2
      (count
       (filter
        (fn [count]
          (= 2 (second count)))
        (frequencies
         (map rank hand)))))))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        start  (apply min sorted)
        has-ace? (= (apply max sorted) 14)]
    (if
     (= (range start
               (+ 5 start))
        sorted) true
        (if has-ace?
          (let [alt-sorted (sort (replace {14 1} sorted))]
            (= (range 1 6)
               alt-sorted))
          false))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush?    hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max
     (sort
      (map (fn [checker]
             (if ((first checker) hand)
               (second checker)
               0))
           checkers)))))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
