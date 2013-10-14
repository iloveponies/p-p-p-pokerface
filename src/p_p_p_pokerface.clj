(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank suit] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (get replacements rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn n-same-cards-by [typ cards n]
  (let [values (map typ cards)
        freqs (frequencies values)
        same? (fn [key] (== n (get freqs key)))]
    (filter same? (keys freqs))))

(defn n-same-ranks [cards n]
  (n-same-cards-by rank cards n))
  ;; (let [ranks (map rank cards)
  ;;       freqs (frequencies ranks)
  ;;       same? (fn [key] (== n (get freqs key)))]
  ;;   (filter same? (keys freqs))))
(defn n-same-suits [cards n]
  (n-same-cards-by suit cards n))

(defn pair? [hand]
  (== 1 (count (n-same-ranks hand 2))))

(defn three-of-a-kind? [hand]
  (== 1 (count (n-same-ranks hand 3))))

(defn four-of-a-kind? [hand]
  (== 1 (count (n-same-ranks hand 4))))

(defn flush? [hand]
  (== 1 (count (n-same-suits hand 5))))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (== 2 (count (n-same-ranks hand 2))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (or
      (apply < ranks)
      (and 
        (== 14 (last ranks))
        (== 2 (first ranks))
        (apply < (sort (assoc ranks 4 1)))))))


(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))

(defn value [hand]
  nil)
