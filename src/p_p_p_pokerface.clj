(ns p-p-p-pokerface)

(defn rank [card]
  (let [[snd _]card]
    (if(Character/isDigit snd)(Integer/valueOf (str snd))(get {\A 14, \K 13, \Q 12, \J 11, \T 10} snd)))
  )

;(rank "KH")

(defn suit [card]
  (let [[_ snd]card]
    (str snd))
  )

;(suit "2H")

(defn pair? [hand]
  (let [freqs (frequencies (map rank hand))
        most (vals freqs)]
    (if(= (apply max most)2)true false)))

;(pair-hand? ["2H" "2S" "3C" "5C" "4D"])

(defn three-of-a-kind? [hand]
  (let [freqs (frequencies (map rank hand))
        most (vals freqs)]
    (if(= (apply max most)3)true false)))

(defn four-of-a-kind? [hand]
 (let [freqs (frequencies (map rank hand))
        most (vals freqs)]
    (if(= (apply max most)4)true false)))

(defn flush? [hand]
  (let [freqs (frequencies (map suit hand))
        most (vals freqs)]
    (if(= (apply max most)5)true false)))


(defn full-house? [hand]
  (let [freqs (frequencies (map rank hand))
        most (str(vals freqs))]
    (if(= most "(3 2)" )true false)))

;(full-house? ["2H" "2S" "2C" "5C" "5D"])

(defn two-pairs? [hand]
 (let [freqs (frequencies (map rank hand))
        most (str(vals freqs))]
    (if(= most "(2 2 1)" )true false)))


(defn straight? [hand]
  (let [ranksies (map rank hand)
        ordered (sort ranksies)]
    (if(apply < ordered) (if(= (+ (first ordered) 4)(last ordered))true
                           (if(and (= (last ordered) 14)(= (first ordered) 2))true false)) false))
  )

;(straight? ["4H" "2S" "6C" "5C" "AD"])

(defn straight-flush? [hand]
  (if(and (flush? hand) (straight? hand))true false)
  )

;(straight-flush? ["2H" "3H" "6H" "5H" "4H"])


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

   :else 0
))

;(value ["2H" "2S" "2C" "2C" "7D"])

