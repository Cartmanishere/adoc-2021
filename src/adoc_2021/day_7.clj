(ns adoc-2021.day-7
  (:require [aocd.core :as data]
            [clojure.string :as s]))

(def input (delay (data/input 2021 7)))

;; ──────────────────────────── Load data ───────────────────────────

(defn load-data
  [inpt]
  (-> inpt
      s/trim-newline
      (s/split #",")
      ((partial map #(Integer/parseInt %)))))

(defn dist-map
  [nums]
  (let [mx (apply max nums)]
    (into {} (map (fn [x]
                    [x 0])
               (range (inc mx))))))

;; Brute force.
(defn calc-fuel
  [burn-fn nums]
  (let [dmap (dist-map nums)]
    (reduce (fn [dmap [pos dest]]
              (update dmap dest + (burn-fn pos dest)))
            dmap
            (for [pos nums
                  dest (keys dmap)]
              [pos dest]))))

(defn get-min-fuel
  [inpt burn-fn]
  (->> inpt
       load-data
       (calc-fuel burn-fn)
       vals
       (filter (complement zero?))
       (apply min)))

;; ────────────────────────── For solution 1 ──────────────────────────

(defn burn-fuel-constant
  [pos dest]
  (Math/abs (- pos dest)))

(defn soln-1
  []
  (get-min-fuel @input burn-fuel-constant))

;; ────────────────────────── For solution 2 ──────────────────────────

(defn burn-fuel-dyn
  [pos dest]
  (let [n (Math/abs (- pos dest))]
    (/ (* n (inc n)) 2)))

(defn soln-2
  []
  (get-min-fuel @input burn-fuel-dyn))
