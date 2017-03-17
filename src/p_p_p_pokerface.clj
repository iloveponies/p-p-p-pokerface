(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (if (Character/isDigit (let [[fst _] card] fst))
    (Integer/valueOf (str (let [[fst _] card] fst)))
    (get replacements (let [[fst _] card] fst))))

(defn suit [card]
  (str (let [[_ snd] card] snd)))


(defn pair? [hand]
 (let [[fst snd thrd frth ffth] hand
       value (apply max (vals (frequencies [(rank fst) (rank snd) (rank thrd) (rank frth) (rank ffth)])))]
  (if (== 2 value)
   true
   false)))


(defn three-of-a-kind? [hand]
  (let [[fst snd thrd frth ffth] hand
        value (apply max (vals (frequencies [(rank fst) (rank snd) (rank thrd) (rank frth) (rank ffth)])))]
   (if (== 3 value)
     true
     false)))

(defn four-of-a-kind? [hand]
 (let [[fst snd thrd frth ffth] hand
       value (apply max (vals (frequencies [(rank fst) (rank snd) (rank thrd) (rank frth) (rank ffth)])))]
  (if (== 4 value)
    true
    false)))

(defn flush? [hand]
  (let [[fst snd thrd frth ffth] hand
        value (apply max (vals (frequencies [(suit fst) (suit snd) (suit thrd) (suit frth) (suit ffth)])))]
   (if (== 5 value)
     true
     false)))

(defn full-house? [hand]
  (let [[fst snd thrd frth ffth] hand
        seq (sort (vals (frequencies [(rank fst) (rank snd) (rank thrd) (rank frth) (rank ffth)])))]
   (if (= [2 3] seq)
     true
     false)))

(defn two-pairs? [hand]
  (let [[fst snd thrd frth ffth] hand
        seq (sort (vals (frequencies [(rank fst) (rank snd) (rank thrd) (rank frth) (rank ffth)])))]
   (if (= [1 2 2] seq)
     true
     false)))

(defn straight? [hand]
  (let [[fst snd thrd frth ffth] hand
        seq (sort (keys (frequencies [(rank fst) (rank snd) (rank thrd) (rank frth) (rank ffth)])))]
   (cond
     (= seq (range (apply min seq) (+ (apply min seq) 5))) true
     (= seq [2 3 4 5 14]) true
     :else false)))

(defn straight-flush? [hand]
  (if (and (flush? hand) (straight? hand))
    true
    false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checker #{[high-card? 0] [pair? 1] [two-pairs? 2]
                  [three-of-a-kind? 3] [flush? 5] [straight? 4]
                  [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}
        func (fn [v] (v hand))
        filt (fn [v] (func (first v)))]
   (apply max (map second (filter filt checker)))))

