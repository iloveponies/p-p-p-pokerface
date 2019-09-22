(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn num-of-kind? [hand nb]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (contains? (set freq) nb)))

(defn pair?
  "Comment"
  [hand]
  (num-of-kind? hand 2))

(defn three-of-a-kind? [hand]
  (num-of-kind? hand 3))

(defn four-of-a-kind? [hand]
  (num-of-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        freq (frequencies suits)]
    (= (count freq) 1)))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))
        sfreq (sort freq)]
    (or
     (= '(1 2 2) sfreq)
     (= '(1 4) sfreq))))

(defn straight-from-min-max [ranks]
  (let [range-min (apply min ranks)
        range-max (apply max ranks)]
    (range range-min (+ 1 range-max))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        high-ace-ranks (replace {1 14} ranks)
        low-ace-ranks (replace {14 1} ranks)
        high-ace-straight (straight-from-min-max high-ace-ranks)
        low-ace-straight (straight-from-min-max low-ace-ranks)]
    (or
     (= (sort high-ace-ranks) high-ace-straight)
     (= (sort low-ace-ranks) low-ace-straight))))

(defn straight-flush? [hand]
  (and
   (flush? hand)
   (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        apply-checker (fn [x] (if ((first x) hand) (second x) -1))
        values (map apply-checker checkers)]
  (apply max values)))

; '_______'
