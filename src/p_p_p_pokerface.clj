(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (cond
      (Character/isDigit fst) (Integer/valueOf (str fst))
      :else (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [values (map rank hand)]
  (<= 2 (apply max (vals (frequencies values))))))

(defn three-of-a-kind? [hand]
  (let [values (map rank hand)]
  (<= 3 (apply max (vals (frequencies values))))))

(defn four-of-a-kind? [hand]
  (let [values (map rank hand)]
  (<= 4 (apply max (vals (frequencies values))))))

(defn flush? [hand]
  (let [suits (map suit hand)]
  (= 5 (apply max (vals (frequencies suits))))))

(defn full-house? [hand]
  (let [values (map rank hand)]
  (and (= 3 (apply max (vals (frequencies values))))
       (= 2 (apply min (vals (frequencies values)))))))

(defn two-pairs? [hand]
  (let [values (map rank hand)]
    (or (four-of-a-kind? hand) (and (= 2 (apply max (vals (frequencies values))))
                                    (= 3 (count (vals (frequencies values)))))
    )))

(defn straight? [hand]
  (let [values (map rank hand)
        mx (apply max values)
        mn (apply min values)
        replaced (replace {14 1} values)]
        (cond
          (= mx 14) (or (and (= 4 (- mx mn)) (= 1 (apply max (vals (frequencies values)))))(and (= 4 (- (apply max replaced) (apply min replaced)))
                             (= 1 (apply max (vals (frequencies replaced))))))
          :else (and (= 4 (- mx mn)) (= 1 (apply max (vals (frequencies values))))))
    ))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (cond
    (straight-flush? hand) (int 8)
    (four-of-a-kind? hand) (int 7)
    (full-house? hand) (int 6)
    (flush? hand) (int 5)
    (straight? hand) (int 4)
    (three-of-a-kind? hand) (int 3)
    (two-pairs? hand) (int 2)
    (pair? hand) (int 1)
    :else (int 0))
  ;;(apply max (map (filter (fn [x] (x))checkers) hand))
  ))
