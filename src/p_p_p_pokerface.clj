(ns p-p-p-pokerface)

; in buffer use C-c M-n to change namespace
; C-c C-k to compile code to repl
(defn rank [param]
  (let [[r _] param] (str r)))

(defn suit [card]
  (let [[_ c] card ] (str c)))

(defn x-of-a-kind? [x hand]
  (<= x (apply max (into [] (vals (frequencies (map rank hand)))))))

(defn pair? [hand]
  (<= 2 (apply max (into [] (vals (frequencies (map rank hand)))))))


(defn three-of-a-kind? [hand]
  (x-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (x-of-a-kind? 4 hand))

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

(println "hello")

