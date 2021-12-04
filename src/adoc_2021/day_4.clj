(ns adoc-2021.day-4
  (:require [aocd.core :as data]
            [clojure.string :as s]))

(def input (delay (data/input 2021 4)))

;; ──────────────────────────── Load data ───────────────────────────

(defn- gen-board-map
  "Take a board layout and generate a mapping between element and its
  location on the board."
  [layout]
  (let [len (count layout)]
    (reduce (fn [board [row col]]
              (assoc board (nth (nth layout row) col) [row col]))
            {}
            (for [row (range len)
                  col (range len)]
              [row col]))))

(defn construct-board
  "Parse the board string into a mapping from number to coordinate on
  the board."
  [board-str]
  (let [layout (map #(s/split (s/trim %) #"  ?")
                    (s/split-lines board-str))
        board (gen-board-map layout)]
    {:board-map board
     :len (count layout)
     :marked #{}}))

(defn load-data
  [inpt]
  (let [parts (s/split inpt #"\n\n")]
    {:boards (map construct-board (rest parts))
     :draws (s/split (first parts) #",")}))

(defn bingo?
  "Check if a board has bingo."
  [{:keys [marked len]}]
  (let [horizontal (frequencies (map first marked))
        vertical (frequencies (map second marked))
        check-bingo #(when (seq %) (= (apply max %) len))]
    (boolean (or (check-bingo (vals horizontal))
                 (check-bingo (vals vertical))))))

(defn mark-board
  [{:keys [board-map marked] :as board} n]
  (assoc board
         :marked (conj marked (get board-map n))))

(defn calc-score
  "Calculate the score of the winning board."
  [{:keys [board-map marked]} n]
  (* (Integer/parseInt n)
     (reduce-kv (fn [acc num coord]
               (if (contains? marked coord)
                 acc
                 (+ acc (Integer/parseInt num))))
             0
             board-map)))

;; ────────────────────────── For solution 1 ──────────────────────────

(defn bingo-loop
  "Play the bingo until one of the boards hits a bingo."
  [boards draws prev]
  (if-let [winner (seq (filter bingo? boards))]
    {:winner (into {} winner)
     :num prev}
    (bingo-loop (map #(mark-board % (first draws)) boards)
                (rest draws)
                (first draws))))

(defn play
  [{:keys [boards draws]}]
  (let [{:keys [winner num]} (bingo-loop boards draws nil)]
    (calc-score winner num)))

;; ────────────────────────── For solution 2 ──────────────────────────

(defn bingo-loop-lose
  "Play bingo until you find the last winner."
  [boards draws prev]
  (let [results (group-by bingo? boards)
        winners (get results true)
        losers (get results false)]
    (if (not (seq losers))
      {:winner (first winners)
       :num prev}
      (bingo-loop-lose (map #(mark-board % (first draws)) losers)
                       (rest draws)
                       (first draws)))))

(defn play-to-lose
  [{:keys [boards draws]}]
  (let [{:keys [winner num]} (bingo-loop-lose boards draws nil)]
    (calc-score winner num)))
