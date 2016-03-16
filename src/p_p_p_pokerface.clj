(ns p-p-p-pokerface)

(defn rank [card]
  (let [digit (first card)
        replacements {\A 14
                      \K 13
                      \Q 12
                      \J 11
                      \T 10}]
    (if (Character/isDigit digit)
      (Integer/valueOf (str digit))
      (replacements digit))))

(defn suit [card]
  (str (second card)))

(defn digits-by [hand]
  (map rank hand))

(defn quantity-of-a-kind? [hand quantity]
  (let [quantities (frequencies (digits-by hand))]
    (>= (apply max (vals quantities)) quantity)))

(defn pair? [hand]
  (quantity-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (quantity-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (quantity-of-a-kind? hand 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [quantities (frequencies (digits-by hand))]
    (= #{2 3} (set (vals quantities)))))

(defn two-pairs? [hand]
  (let [quantities (frequencies (digits-by hand))]
    (= [1 2 2] (sort (vals quantities)))))

(defn straight? [hand]
  (let [digits  (digits-by hand)
        hand-14 (sort digits)
        min-14  (first hand-14)
        seq-14  (range min-14 (+ 5 min-14))
        hand-1  (sort (replace {14 1} digits))
        min-1   (first hand-1)
        seq-1   (range min-1 (+ 5 min-1))]
    (or (= hand-14 seq-14)
        (= hand-1 seq-1))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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