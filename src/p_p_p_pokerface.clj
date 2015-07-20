(ns p-p-p-pokerface)

(defn rank [card]
  (let [[charrank _] card]
  (if (Character/isDigit charrank)
    (Integer/valueOf (str charrank))
    (get {\T 10, \J 11, \Q 12, \K 13, \A 14} charrank))))

(defn suit [card]
  (let [[_ suit] card]
  (str suit)))

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [acehigh (sort (map rank hand))
        acelow (sort (replace {14 1} (map rank hand)))
        almin (apply min acelow)
        almax (apply max acelow)
        ahmin (apply min acehigh)
        ahmax (apply max acehigh)]
        (or (= (range almin (+ 1 almax)) acelow)
            (= (range ahmin (+ 1 ahmax)) acehigh))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4]  [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matchedhands (filter (fn [x] ((first x) hand)) checkers)]
    (apply max (map second matchedhands))))

