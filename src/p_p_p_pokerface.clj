(ns p-p-p-pokerface)

(defn rank [card]
  (let [r (first card)]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} r))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (let [ranks (map rank hand)            ; ranks of the hand in a list, e.e. (J J A Q A)
        frqs (frequencies ranks)         ; frequencies of the ranks in a map, e.g. {J 2 Q 1 A 2}
        sames (vals frqs)                ; values of those, e.g. (2 1 2)
        same-frqs (frequencies sames)]   ; frequencies of those, e.g. {2 2 1 1}
    (= 1 (get same-frqs 2))))            ; Check if there's exactly one pair. (I'll use this method later too)

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        frqs (frequencies ranks)
        sames (vals frqs)]
    (boolean (and (some #{3} sames) (not (some #{2} sames))))))   ;contains? doesn't work with sequences. Google and cheat-sheet helped me out here.
                                                                  ;I took care to understand what happens here, so it isn't cheating. :)

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        frqs (frequencies ranks)
        sames (vals frqs)]
    (boolean (some #{4} sames))))

(defn flush? [hand]
  (let [suits (map suit hand)
        frqs (frequencies suits)]
    (== 1 (count frqs))))       ; This accepts straight flush too.

(defn full-house? [hand]
  (let [ranks (map rank hand)
        frqs (frequencies ranks)
        sames (vals frqs)]
    (boolean (and (some #{3} sames) (some #{2} sames)))))

(defn two-pairs? [hand]
   (let [ranks (map rank hand)
        frqs (frequencies ranks)
        sames (vals frqs)
        same-frqs (frequencies sames)]
    (or (= 2 (get same-frqs 2)) (= 1 (get same-frqs 4)))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        a-low (replace {14 1} ranks)
        ordered (sort ranks)
        ordered-2 (sort a-low)
        low-card (apply min ordered)
        low-card-2 (apply min ordered-2)]
    (or (= ordered (range low-card (+ low-card 5)))
        (= ordered-2 (range low-card-2 (+ low-card-2 5))))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0))
