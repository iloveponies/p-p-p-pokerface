(ns p-p-p-pokerface)

(defn rank [card]
  (let [[char] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit char)
      (Integer/valueOf (str char))
      (get replacements char))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= 2 (apply max freqs))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= 3 (apply max freqs))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= 4 (apply max freqs))))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (= 5 (apply max freqs))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= [2 3] (sort freqs))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        two (= [1 2 2] (sort freqs))]
    (or two (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [nums (map rank hand)
        small (apply min nums)
        ranks (sort nums)
        ranks2 (sort (replace {14 1} ranks))
        s (map + (range 5) (iterate identity small))]
    (or (= ranks s) (= ranks2 [1 2 3 4 5]))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                 [two-pairs? 2]   [three-of-a-kind? 3]
                 [straight? 4]    [flush? 5]
                 [full-house? 6]  [four-of-a-kind? 7]
                 [straight-flush? 8]}
        funcs (filter (fn [func] ((first func) hand)) checkers)
        vals (map second funcs)]
    (apply max vals)))
