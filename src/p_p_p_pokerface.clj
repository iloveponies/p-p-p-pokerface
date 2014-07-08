(ns p-p-p-pokerface)

(def character-rank->integer {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq ))

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (character-rank->integer fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-of-a-kind? [hand n]
  (boolean (in? (vals (frequencies (map rank hand))) n)))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [ranks (map rank hand)]
    (and (apply = (map suit hand)) (< 4 (- (apply max ranks) (apply min ranks))))))

(defn full-house? [hand]
  (and (n-of-a-kind? hand 2) (n-of-a-kind? hand 3)))

(defn two-pairs? [hand]
  (= 2 (count (filter #{2} (vals (frequencies (map rank hand)))))))

(defn sequential-hand?
  "Takes a hand and returns true if the ranks are sequential"
  [ranks]
  (let [ranks (sort ranks)]
    (= (range (apply min ranks) (+ 1 (apply max ranks))) ranks)))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (if (boolean (in? ranks 14))
      (or (sequential-hand? ranks) (sequential-hand? (replace {14 1} ranks)))
      (sequential-hand? ranks))))

(defn straight-flush? [hand]
  (and (or (flush? hand) (apply = (map suit hand))) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [checker] ((first checker) hand)) checkers)))))
