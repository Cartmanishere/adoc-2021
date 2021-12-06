(ns adoc-2021.day-6
  (:require [aocd.core :as data]
            [clojure.string :as s]))

(def input (delay (data/input 2021 6)))

;; ──────────────────────────── Load data ───────────────────────────

(defn load-data
  [inpt]
  (-> inpt
      s/trim-newline
      (s/split #",")
      ((partial map #(Integer/parseInt %)))))

;; ────────────────────────────── Common ──────────────────────────────

(defn build-fish
  [fish]
  (reduce (fn [fish-school [days cnt]]
            (update fish-school
                    days
                    (constantly cnt)))
          (into [] (repeat 9 0))
          (frequencies fish)))

(defn inc-day
  [f-counts]
  (let [new-fish (first f-counts)
        old-fish (into [] (rest f-counts))]
    (conj (update old-fish 6 + new-fish)
          new-fish)))

(defn count-fish
  [fish days]
  (apply + (reduce (fn [acc _]
                     (inc-day acc))
                   (build-fish fish)
                   (range days))))

;; ────────────────────────── For solution 1 ──────────────────────────

(defn soln-1
  []
  (count-fish (load-data @input) 80))

;; ────────────────────────── For solution 2 ──────────────────────────

(defn soln-2
  []
  (count-fish (load-data @input) 256))
