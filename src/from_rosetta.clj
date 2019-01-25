(ns from-rosetta)

;; from https://rosettacode.org/wiki/Poker_hand_analyser#Clojure

;; with some minor changes, for readability.

(defn rank [[fst _]]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    ({\T 10, \J 11, \Q 12, \K 13, \A 14} fst)))

(defn suit [[_ snd]]
  (str snd))

(defn n-of-a-kind [hand n]
  (->> hand
       (map rank)
       frequencies
       vals
       (map #(>= % n))
       (filter #(= true %))
       empty?
       not))

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (->> hand
       (map suit)
       frequencies
       vals
       (map #(>= % 5))
       (filter #(= true %))
       empty?
       not))

(defn full-house? [hand]
  (true? (and
    (some #(= 2 %) (vals (frequencies (map rank hand))))
    (some #(= 3 %) (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (or
    (full-house? hand)
    (four-of-a-kind? hand)
    (= 2 (->> hand
              (map rank)
              frequencies
              vals
              (map #(>= % 2))
              (filter #(= true %))
              count))))

(defn ranks-with-ace [hand]
  (let [ranks (sort (map rank hand))]
    (if (some #(= 14 %) ranks)
      (cons 1 ranks)
      ranks)))

(defn straight? [hand]
  (let [[fst snd :as hand-a] (ranks-with-ace hand)]
    (or
     (= (take 5 hand-a) (range fst (+ fst 5)))
     (= (drop 1 hand-a) (range snd (+ snd 5))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn invalid? [hand]
  (not= 5 (count (set hand))))

(defn check-hand [hand]
  (cond
    (invalid? hand)         "invalid"
    (straight-flush? hand)  "straight-flush"
    (four-of-a-kind? hand)  "four-of-a-kind"
    (full-house? hand)      "full-house"
    (flush? hand)           "flush"
    (straight? hand)        "straight"
    (three-of-a-kind? hand) "three-of-a-kind"
    (two-pairs? hand)       "two-pair"
    (pair? hand)            "one-pair"
    :else                   "high-card"))

; Test examples
(def hands [["2H" "2D" "2S" "KS" "QD"]
            ["2H" "5H" "7D" "8S" "9D"]
            ["AH" "2D" "3S" "4S" "5S"]
            ["2H" "3H" "2D" "3S" "3D"]
            ["2H" "7H" "2D" "3S" "3D"]
            ["2H" "7H" "7D" "7S" "7C"]
            ["TH" "JH" "QH" "KH" "AH"]
            ["4H" "4C" "KC" "5D" "TC"]
            ["QC" "TC" "7C" "6C" "4C"]])

(run! println (map #(str % " : " (check-hand %)) hands))
