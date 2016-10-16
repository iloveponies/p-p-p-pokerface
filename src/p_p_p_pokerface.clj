(ns p-p-p-pokerface)

(defn in? [collection element]
  (contains? (set collection) element))

(defn rank [card]
  (let [[rank _] card
        numeric? (Character/isDigit rank)
        special-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
   (if numeric?
     (Integer/valueOf (str rank))
     (get special-ranks rank))))

(defn suit [card]
  (let [[_ suit] card] (str suit)))

(defn n-of-a-kind? [hand n]
  (let [freqs (vals (frequencies (map rank hand)))]
   (in? freqs n)))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
   (apply = suits)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
