(ns p-p-p-pokerface)

(def rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14})

;rank -> numero do carta
;suit -> simbolo

(defn rank [[r _]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (get rank-map r)))

(defn suit [[_ s]]
  (str s))

(defn- n-of-same-rank-plus-and-others-different [hand n]
  (-> (map rank hand) frequencies vals distinct sort (= [1 n])))

(defn pair? [hand]
  (n-of-same-rank-plus-and-others-different hand 2))

(defn three-of-a-kind? [hand]
  (n-of-same-rank-plus-and-others-different hand 3))

(defn four-of-a-kind? [hand]
  (n-of-same-rank-plus-and-others-different hand 4))

(defn flush? [hand]
  (-> (map suit hand) distinct count (= 1)))

(defn full-house? [hand]
  (-> (map rank hand) frequencies vals distinct sort (= [2 3])))

(defn two-pairs? [hand]
  (-> (map rank hand) frequencies vals sort (= [1 2 2])))

(defn straight? [hand]
  (let [rank-sorted (-> (map rank hand) sort)
        rank-sorted-as-as-one (if (some #{14} rank-sorted)
                                (-> (replace {14 1} rank-sorted) sort)
                                rank-sorted)]
    (or (apply = 1 (map - (rest rank-sorted) rank-sorted))
        (apply = 1 (map - (rest rank-sorted-as-as-one)  rank-sorted-as-as-one)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [checkers #{[pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (-> (map #(if ((first %) hand) (second %) 0) checkers) sort last)))
