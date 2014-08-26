(ns p-p-p-pokerface)

(def ranks-mapping {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[r _] card
        rs (str r)]
    (if (Character/isDigit r)
        (Integer/valueOf rs)
        (ranks-mapping r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))
        idx (.indexOf freq 2)
        pairs-number ((frequencies freq) 2)]
    (and
       (not= -1 idx)
       (= 1 pairs-number))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))
        idx (.indexOf freq 3)
        threes-number ((frequencies freq) 3)]
    (and
       (not= -1 idx)
       (= 1 threes-number))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))
        idx (.indexOf freq 4)
        fours-number ((frequencies freq) 4)]
    (and
       (not= -1 idx)
       (= 1 fours-number))))

(defn flush? [hand]
  (let [suits (map suit hand)
        rep (first (vals (frequencies suits)))]
    (if (= rep 5)
      true
      false)))

(defn full-house? [hand]
  (and
     (pair? hand)
     (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
     (let [ranks (map rank hand)
        freq (vals (frequencies ranks))
        idx (.indexOf freq 2)
        pairs-number ((frequencies freq) 2)]
       (and
         (not= -1 idx)
         (= 2 pairs-number)))
     (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [mranks (sort (map rank hand))
        ranks (sort (if (and
                     (= 14 (nth mranks 4))
                     (not= 13 (nth mranks 3)))
                  (seq (replace {14 1} mranks))
                  (do mranks)))
        min (apply min ranks)
        mirror (range min (+ min 5))]
     (= ranks mirror)))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  (not(or (straight-flush? hand)
          (straight? hand)
          (two-pairs? hand)
          (full-house? hand)
          (flush? hand)
          (four-of-a-kind? hand)
          (three-of-a-kind? hand)
          (pair? hand))))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2]
                   [three-of-a-kind? 3] [four-of-a-kind? 7] [straight? 4]
                   [flush? 5] [full-house? 6] [straight-flush? 8]}
        matching (map (fn [[fun vl]] (
                                      if (fun hand)
                                        (do vl)
                                        (do -1))) checkers)]
    (apply max matching)))



