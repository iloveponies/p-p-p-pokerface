(ns p-p-p-pokerface)

(defn rank [card]
  (let [facemap {\J 11,\Q 12
                 \K 13,\A 14
                 \T 10,}
        [fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get facemap fst))))
  
(defn suit [card]
  (let [ [_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [rankhand (map rank hand)
        rankfreq (frequencies rankhand)]
    (<= 2 (apply max (vals rankfreq)))))

(defn three-of-a-kind? [hand]
  (let [rankhand (map rank hand)
        rankfreq (frequencies rankhand)]
    (<= 3 (apply max (vals rankfreq)))))

(defn four-of-a-kind? [hand]
  (let [rankhand (map rank hand)
        rankfreq (frequencies rankhand)]
    (= 4 (apply max (vals rankfreq)))))

(defn flush? [hand]
  (let [suithand (map suit hand)
        suitfreq (frequencies suithand)]
    (= 5 (apply max (vals suitfreq)))))

(defn full-house? [hand]
  (let [rankhand (map rank hand)
        rankfreq (frequencies rankhand)]
    (= [2 3] (sort (vals rankfreq)))))

(defn two-pairs? [hand]
  (let [rankhand (map rank hand)
        rankfreq (frequencies rankhand)
        rankfreqvals (sort (vals rankfreq))]
    (or 
      (= [1 2 2] rankfreqvals)
      (= [1 4] rankfreqvals))))

(defn straight? [hand]
  (let [rank-hand-a-high (map rank hand)
        [fst _] rank-hand-a-high
        rank-hand-a-low (replace {14 1} rank-hand-a-high)]
    (or
      (= (range fst (+ fst 5)) (sort rank-hand-a-high))
      (= (range 1 6) (sort rank-hand-a-low)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
(let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
      has-combination? (fn [matcher]
                          ((first matcher) hand))]
  (apply max (map second (filter has-combination? checkers)))))