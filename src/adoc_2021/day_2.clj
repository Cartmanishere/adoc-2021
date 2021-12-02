(ns adoc-2021.day-2
  (:require [aocd.core :as data]
            [clojure.string :as s]))

(def input (delay (data/input 2021 2)))

(defn get-dirs
  "Process the input to generate the list of dirs"
  [inpt]
  (->> (s/split-lines inpt)
       (map #(s/split % #" "))
       (map (fn [[dir val]] [dir (Integer/parseInt val)]))))

;; ────────────────────────── For Solution 1 ──────────────────────────

(defn calc-position
  "Take a list of directions and calculate the final position."
  [dirs]
  (reduce (fn [[h d] [direction val]]
            (case direction
              "forward" [(+ h val) d]
              "down"    [h (+ d val)]
              "up"      [h (- d val)]))
          [0 0]
          dirs))

(defn soln-1
  []
  (let [dirs (get-dirs @input)
        position (calc-position dirs)]
    (apply * position)))

;; ────────────────────────── For solution 2 ──────────────────────────

(defn calc-position-2
  "Take a list of directions and calculate the final position."
  [dirs]
  (reduce (fn [[h d a] [direction val]]
            (case direction
              "forward" [(+ h val) (+ d (* a val)) a]
              "down"    [h d (+ a val)]
              "up"      [h d (- a val)]))
          [0 0 0]
          dirs))

(defn soln-2
  []
  (let [dirs (get-dirs @input)
        position (calc-position-2 dirs)]
    (apply * (take 2 position))))
