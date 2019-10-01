(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        tbl {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/parseInt (str r))
      (get tbl r))))

(defn suit [card]
  (let [[_ s] card] 
    (str s)))

(defn freqs [ranks]
  (vals (frequencies ranks)))

(defn x-of-a-kind? [x hand]
  (let [ranks (map rank hand)]
    (= (apply max (freqs ranks)) x)))

(defn pair? [hand]
  (x-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (x-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (x-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= (set (freqs ranks)) #{2 3})))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (= (count (filter #{2} (freqs ranks))) 2)))

(defn flip-ace [ranks]
  (replace {14 1} ranks))

(defn straight? [hand]
  (let [ranks (map rank hand)
        check #(= (sort %) 
                  (range (apply min %) 
                         (inc (apply max %))))]
    (or (check (flip-ace ranks))
        (check ranks))))

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
        check (fn [checker] 
                (let [[fun res] checker]
                  (if (fun hand) res nil)))]
    (apply max (filter #(not= % nil) (map check checkers)))))
