(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [ [r _] card]
    (if (not= nil (replacements r))
      (replacements r)
      (Integer/valueOf (str r)))))

(defn suit [card]
  (let [ [_ s] card]
    (str s)))

(defn pair? [hand]
  (let [cards (map rank hand)]
    (.contains (vals (frequencies cards)) 2)))

(defn three-of-a-kind? [hand]
  (let [cards (map rank hand)]
    (.contains (vals (frequencies cards)) 3)))

(defn four-of-a-kind? [hand]
  (let [cards (map rank hand)]
    (.contains (vals (frequencies cards)) 4)))

(defn flush? [hand]
  (let [cards (map suit hand)]
    (.contains (vals (frequencies cards)) 5)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [hand-ranks (map rank hand)]
    (= 2 (get (frequencies (vals (frequencies hand-ranks))) 2))))

;; if there is \A, add rank 1

(defn straight? [hand]
  (let [hand-ranks (map rank hand)]
    ))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
