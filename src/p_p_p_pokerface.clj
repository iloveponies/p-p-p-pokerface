(ns p-p-p-pokerface)

(def ranks {"2" 2
            "3" 3
            "4" 4
            "5" 5
            "6" 6
            "7" 7
            "8" 8
            "9" 9
            "T" 10
            "J" 11
            "Q" 12
            "K" 13
            "A" 14})

(defn rank [[r _]]
  (ranks (str r)))

(defn suit [[_ s]]
  (str s))

(defn times-of-a-kind [num hand]
  (= num (apply max
     (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (times-of-a-kind 2 hand))

(defn three-of-a-kind? [hand]
  (times-of-a-kind 3 hand))

(defn four-of-a-kind? [hand]
  (times-of-a-kind 4 hand))

(defn flush? [hand]
  (= 5 (apply max
     (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= '(2 3)
     (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
