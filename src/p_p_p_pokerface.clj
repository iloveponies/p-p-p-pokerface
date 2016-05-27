(ns p-p-p-pokerface)


(defn rank [card] (let [hiCards {\T 10 \J 11 \Q 12 \K 13 \A 14}
                       [rnk _] card] (if (Character/isDigit rnk) (Integer/valueOf (str rnk)) (Integer/valueOf (str (get hiCards rnk))))))

;  nil)

(defn suit [card] (let [[_ swt] card] (str swt)))
;  nil)

(defn match-n [hand n] (= true (some #(= n %) (vals (frequencies (map rank hand))))))

(defn pair? [hand] (and (match-n hand 2) (= (count (vals (frequencies (map rank hand)))) 4)))
;  nil)

(defn three-of-a-kind? [hand] (and (match-n hand 3) (= (count (vals (frequencies (map rank hand)))) 3)))
;  nil)

(defn four-of-a-kind? [hand] (match-n hand 4))
;  nil)

(defn full-house? [hand] (and (match-n hand 3) (= (count (vals (frequencies (map rank hand)))) 2)))
;  nil)

(defn two-pairs? [hand] (let [pairs (vals (frequencies (map rank hand)))
                              val1 (first pairs)
                              val2 (second pairs)]
                          (or (= 2 val1 val2)
                              (= 4 val1))))

;  nil)

(defn in-straight? [hand] (let [cardval (set (map rank hand))]   ; create sorted set of integers
                         (if (= (count cardval) 5)                                       ; must have 5 different rank values to be a straight
                           (let [lo-card (first cardval)                                 ; integer set is sorted so lo-card and hi-card
                                 hi-card (last cardval)]                                 ; will always contain the low & high card ranks
                             (if (= (- hi-card lo-card) 4) true                          ; will catch any straight other than ace-low
                               (if (and (= hi-card 14)                                   ; if hi-card is an ace then subtract low-ace
                                        (= (- (first (take-last 2 cardval)) 1) 4)) true  ; from next highest card to find low-ace-straight
                                  false))) false)))                                      ; not a straight

;  nil)

(defn in-flush? [hand] (let [cards (map suit hand)] (and (apply = cards) (= (count cards) 5)))) ; must be 5 cards of same suit

;  nil)

(defn flush? [hand] (and (in-flush? hand) (not (in-straight? hand))))

(defn straight? [hand] (and (in-straight? hand) (not (in-flush? hand))))


(defn straight-flush? [hand] (and (in-straight? hand) (in-flush? hand)))

;  nil)

(defn value [hand] (if (pair? hand) 1
                     (if (four-of-a-kind? hand) 7
                       (if (two-pairs? hand) 2
                         (if (three-of-a-kind? hand) 3
                           (if (straight? hand) 4
                             (if (flush? hand) 5
                               (if (full-house? hand) 6
                                 (if (four-of-a-kind? hand) 7
                                   (if (straight-flush? hand) 8
                                     0))))))))))
;  nil)
