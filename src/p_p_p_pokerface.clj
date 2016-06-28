(ns p-p-p-pokerface)

(def ranks
  (let [ks (->> [\T \J \Q \K \A]
                  (concat (range 2 10))
                  (map str))
        vs (range 2 15)]
  (zipmap ks vs)))

(defn rank [card]
  (-> card
      (first)
      (str)
      (ranks)))

(defn suit [card]
  (-> card
      (last)
      (str)))

(defn matching-rank-count
  "Checks if there are any ranks that have
   n occurrences in the given hand"
  [hand n]
  (->> hand
       (map rank)
       (frequencies)
       (filter #(= (second %) n))))

(defn pair? [hand]
  (->> (matching-rank-count hand 2)
       (empty?)
       (not)))

(defn three-of-a-kind? [hand]
  (->> (matching-rank-count hand 3)
       (empty?)
       (not)))

(defn four-of-a-kind? [hand]
  (->> (matching-rank-count hand 4)
       (empty?)
       (not)))

(defn flush? [hand]
  (->> hand
       (map suit)
       (apply =)))

(defn full-house? [hand]
  (as-> hand _
       (map rank _)
       (frequencies _)
       (vals _)
       (and (.contains _ 2) (.contains _ 3))))

(defn two-pairs? [hand]
  (->> (matching-rank-count hand 2)
       (count)
       (= 2)))

(defn increment [acc item]
  (if (empty? acc)
    (conj acc {:item item :value 1})
    (let [prev (last acc)]
      (conj acc {:item item :value (- item (prev :item))}))))

(defn straight? [hand]
  (->> hand
       (map rank)
       (sort)
       (reduce increment [])
       (map :value)
       (remove #(or (< % 0) (= % 9))) ; crossing low-high ace
       (apply = 1)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(def scores
  "Map checks to score values"
  (zipmap [pair? two-pairs? three-of-a-kind? straight? flush?
           full-house? four-of-a-kind? straight-flush?]
          (range 1 9)))

(defn score-hand
  "Takes a hand and scoring-pair [pred score]
   and returns 'score' if (pred hand) is true,
   otherwise returns zero."
  [hand scoring]
  (let [fun (first scoring)
        score (last scoring)]
    (if (fun hand)
      score
      0)))

(defn value [hand]
  (->> (map #(score-hand hand %) scores)
       (drop-while zero?)
       (first)
       ((fn [x] (if (nil? x) 0 x)))))

