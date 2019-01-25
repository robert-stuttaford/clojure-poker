(ns poker)

;; Model a deck of cards
;; Start with plain EDN, then generate it
;; Shuffle it

(def deck
  [{:suit :diamond
    :value :ace}
   {:suit :diamond
    :value 2}
   {:suit :diamond
    :value 3}
   {:suit :diamond
    :value 4}
   {:suit :clubs
    :value :ace}
   {:suit :clubs
    :value 2}
   {:suit :clubs
    :value 3}
   {:suit :clubs
    :value 4}])




(def deck
  (for [value (range 1 14)
        suite [:diamonds :hearts :clubs :spades]]
    [(case value
       1  :ace
       11 :jack
       12 :queen
       13 :king
       value)
     suite]))

deck

(def shuffled-deck
  (-> deck
      shuffle
      shuffle
      shuffle))

(def draw-hand
  (fn [deck] ...))

deck
shuffled-deck

;; Draw a hand

(defn draw-hand [deck]
  {:hand (take 5 deck)
   :deck (drop 5 deck)})

(defn make-round [deck]
  (let [first-hand-result      (draw-hand deck)
        deck-after-first-hand  (:deck first-hand-result)
        second-hand-result     (draw-hand deck-after-first-hand)
        deck-after-second-hand (:deck second-hand-result)]
    {:player-1 (:hand first-hand-result)
     :player-2 (:hand second-hand-result)
     :deck     deck-after-second-hand}))

(def first-round
  (make-round shuffled-deck))

(def second-round
  (make-round (:deck first-round)))


deck
shuffled-deck

first-round
second-round

(def hand (:player-2 first-round))

(defn rules [hand]
  (let [values (map first hand)
        suits  (map second hand)
        values-freq (frequencies values)]
       (cond
         (some #(= 3 %) (vals values-freq))
         :three-of-a-kind
         (some #(= 2 %) (vals values-freq))
         :two-of-a-kind)))

(rules [[4 :diamonds] [3 :spades] [3 :diamonds] [7 :spades] [3 :hearts]])

;; Detect poker rules in a hand
;; Find the winner in a set of hands


{:value 4 :suit :diamonds}

{"value" 4 "suit" :diamonds}

(get {"value" 4 "suit" :diamonds} "value")

(:value {:value 4
         :suit  :diamonds})

(namespace :bundle/name)
(name :bundle/name)

:bundle/name :bundle/title

"bundle/name" "bundle/title"

