(ns p-p-p-pokerface)

(def rank-chars "23456789TJQKA")
(def ranks (zipmap rank-chars (range 2 15)))

(defn rank [[rank-char _]]
  (ranks rank-char))

(defn suit [[_ suit]]
  (str suit))

(defn n-of-a-kind [n hand]
  (->> hand
       (map rank)
       frequencies
       vals
       (some #(= % n))
       boolean))

(defn pair? [hand]
  (n-of-a-kind 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (->> hand
       (map rank)
       frequencies
       vals
       (filter (partial = 2))
       count
       (= 2)))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
         wrapped-ranks (sort (replace {14 1} sorted-ranks))
         straight-from (fn [x] (range x (+ x 5)))]
    (or (= sorted-ranks (straight-from (first sorted-ranks)))
        (= wrapped-ranks (straight-from (first wrapped-ranks))))))

(defn straight-flush? [hand]
  (reduce #(and %1 %2)
          ((juxt flush? straight?) hand)))

(defn high-card? [hand]
  true)

(def checker-fns [high-card? pair? two-pairs? three-of-a-kind?
                  straight? flush? full-house? four-of-a-kind?
                  straight-flush?])

(def checkers (zipmap checker-fns (range)))

(defn value [hand]
  (->> checkers
       (filter #((first %) hand))
       (map second)
       (apply max)))
