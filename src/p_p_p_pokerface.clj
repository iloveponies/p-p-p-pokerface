(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (cond (Character/isDigit r) (Integer/valueOf (str r))
          (= \T r) 10
          (= \J r) 11
          (= \Q r) 12
          (= \K r) 13
          (= \A r) 14)))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (= ((frequencies (vals (frequencies (map rank hand)))) 2) 1))

(defn three-of-a-kind? [hand]
  (= ((frequencies (vals (frequencies (map rank hand)))) 3) 1))

(defn four-of-a-kind? [hand]
  (= ((frequencies (vals (frequencies (map rank hand)))) 4) 1))

(defn flush? [hand]
  (= ((frequencies (vals (frequencies (map suit hand)))) 5) 1))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= ((frequencies (vals (frequencies (map rank hand)))) 2) 2))

(defn straight? [hand]
  (let [ranked (map rank hand)
        sortedAceXIV (sort ranked)
        sortedAceI (sort (replace {14 1} ranked))
        diff1 (- (inc (apply max sortedAceXIV)) (apply min sortedAceXIV)) ;; Pienimmän ja suurimman kortin lukuarvon erotus
        diff2 (- (inc (apply max sortedAceI)) (apply min sortedAceI))]    ;;
    (and (= ((frequencies (vals (frequencies (map rank hand)))) 1) 5)     ;; Suorassa kullakin viidestä kortista on eri lukuarvo
         (or (== diff1 5) (== diff2 5)))))                                ;; ja suurimman ja pienimmän lukuarvon erotus on 5

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0
        ))
