(ns p-p-p-pokerface)

; helpers

(defn digit? [d]
  (Character/isDigit d)
)

(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})

; helpers

(defn rank [card]
  (let [[c _] card]
   (cond
     (Character/isDigit  c) (Integer/valueOf (str c))
     :else (replacements c))
    )
)

(defn suit [card]
  (let [[_ snd] card]
  (str snd))
)

(defn pair? [hand]
  (cond
  (< 1 (apply max (vals (frequencies (map rank hand))))) true
    :else false)
)

(defn three-of-a-kind? [hand]
  nil)

(defn four-of-a-kind? [hand]
  nil)

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
