(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
       (Integer/valueOf (str fst))
       (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn kind? [hand n]
  (not (nil? (some #(= n %) (vals (frequencies (map rank hand)))))))

(defn pair? [hand]
  (kind? hand 2))

(defn three-of-a-kind? [hand]
  (kind? hand 3))

(defn four-of-a-kind? [hand]
  (kind? hand 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= 2 (count (filter (fn[x] (= x 2 ))
                        (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand)),
        ranks_ace_low (sort (replace {14 1} ranks))]
    (or (= ranks (range (first ranks) (+ (last ranks) 1)))
        (= ranks_ace_low (range (first ranks_ace_low) (+ (last ranks_ace_low) 1))))))


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                  [pair? 1]
                  [two-pairs? 2]
                  [three-of-a-kind? 3]
                  [straight? 4]
                  [flush? 5]
                  [full-house? 6]
                  [four-of-a-kind? 7]
                  [straight-flush? 8]}
       chks (map (fn[checker] (if ((first checker) hand) (second checker) 0)) checkers)]
    (apply max chks)))
