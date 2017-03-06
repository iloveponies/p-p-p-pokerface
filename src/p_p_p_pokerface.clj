(ns p-p-p-pokerface)

(defn rank [[card-rank]]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit card-rank)
        (Integer/valueOf (str card-rank))
        (replacements card-rank))))

(defn suit [[_ card-suit]]
  (str card-suit))


(defn has-cards? 
  
  "A helper function which compares a frequency of card ranks or card suits in a hand.
  
   Recognized keywords for argument 'cards' are :pair :three-of-kind :four-of-kind :flush 
   :full-house and :two-pairs.
  
   For usage examples see functions pair? three-of-a-kind? four-of-a-kind? flush? full-house?"
  
  [hand cards]
  
  (let [card-sets
        
          ;; Maps keywords to card data. The first entry in the vector is the function to be
          ;; called to determine card type (whether card frequences are counted based on card
          ;; rank or card suite). The rest of the data is the expected card frequencies in
          ;; a sorted (increasing frequency) order.
          
          {:pair           [rank 1 1 1 2]
           :three-of-kind  [rank 1 1 3]
           :four-of-kind   [rank 1 4]
           :flush          [suit 5]
           :full-house     [rank 2 3]
           :two-pairs      [rank 1 2 2]}
          
        card-data
        
          ;; Resolves the card data via a keyword provided as the function argument
          
          (card-sets cards)
        
        card-type
        
          ;; Resolves the card type used to count card frequencies based on the first entry
          ;; of the card-data: either card rank or card suite based frequencies
          
          (first card-data)
        
        all-types 
        
          ;; maps cards in the given hand based on either their rank or suite, using the 
          ;; corresponding card or rank functions
          
          (map card-type hand)
        
        sorted-kind-freq 
        
          ;; counts the frequencies of the cards (either based on card rank or card suite)
          ;; and sorts the card frequencies in increasing order
          
          (sort (vals (frequencies all-types)))]
  
  ;; compares the card frequences in the given hand to the expected card frequencies identified
  ;; by the card-data keyword (:pair :full-house :flush etc.) 
   
  (= sorted-kind-freq (rest card-data))))


(defn pair? [hand]
  (has-cards? hand :pair))

(defn three-of-a-kind? [hand]
  (has-cards? hand :three-of-kind))

(defn four-of-a-kind? [hand]
  (has-cards? hand :four-of-kind))

(defn flush? [hand]
  (has-cards? hand :flush))

(defn full-house? [hand]
  (has-cards? hand :full-house))

(defn two-pairs? [hand]
  (has-cards? hand :two-pairs))


;; Define replacement cards for determining straight hands -- aces can be
;; used either as value 14 or 1 in straight hand.
(def ace-replacement-cards {"AC" "1C", "AS" "1S", "AD" "1D", "AH" "1H"})

(defn straight? [hand]
  
  (let [has-consecutive-ranks? 
        
          ;; Define a temporary function has-consecutive-ranks? which checks if 
          ;; the cards in the hand can be arranged to have consecutive ranks.
          ;;
          ;; The function allows replacement-cards map as an argument which
          ;; can be used to replace ranks of ace cards which allow both rank
          ;; value 1 or rank value 14 depending on the card hand configuration.
          
          (fn [hand replacement-cards] 
            
            (let [sorted-ranks
                  
                    ;; Sort cards in the hand to increasing order according to 
                    ;; their rank -- if a non-empty replacement-cards map is
                    ;; provided, cards in the hand are replaced first before 
                    ;; sorting them.
                    
                    (sort (map rank (replace replacement-cards hand)))
                    
                  min-rank
                  
                    ;; Get the minimum rank card in the sorted card hand.
                    
                    (apply min sorted-ranks)
          
                  normalize-ranks 
                  
                    ;; Map function to normalize card ranks in the hand 
                    ;; according to the minimum card rank -- if the hand 
                    ;; holds cards of consecutive rank we'll end up with
                    ;; a number sequence (0 1 2 3 4).
                    
                    (fn [rank-value] (- rank-value min-rank))]
              
              ;; Map cards in the hand sorted by the rank to normalize their
              ;; values and compare the result to see if they are in 
              ;; consecutive order -- matching sequence (0 1 2 3 4)
              
              (= (map normalize-ranks sorted-ranks) [0 1 2 3 4])))]

  ;; Test if the hand has cards whose rank can be arranged to have a consecutive
  ;; order with an empty replacement-card map (aces as rank value 14) and with
  ;; an ace-replacement-cards which uses rank value 1 for ace cards.
  
  (or (has-consecutive-ranks? hand {}) 
      (has-consecutive-ranks? hand ace-replacement-cards))))

  
(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond 
    (straight-flush? hand)    8
    (four-of-a-kind? hand)    7
    (full-house? hand)        6
    (flush? hand)             5
    (straight? hand)          4
    (three-of-a-kind? hand)   3 
    (two-pairs? hand)         2
    (pair? hand)              1
    :else                     0))

