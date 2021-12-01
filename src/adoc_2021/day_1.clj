(ns adoc-2021.day-1
  (:require [aocd.core :as data]
            [clojure.string :as s]))

(def input (delay (data/input 2021 1)))

(defn count-increase
  "Count the number of times the increase happens in a sequence of numbers."
  [nums]
  (reduce + (map (fn [[a b]]
                   (if (> b a) 1 0))
                 (partition 2 1 nums))))

(defn sum
  [& n]
  (reduce + n))

(defn count-sliding-increase
  "Count the number of times a sliding window's sum increase."
  [nums]
  (reduce + (map (fn [[a b c d]]
                   (if (> (sum b c d)
                          (sum a b c))
                     1
                     0))
                 (partition 4 1 nums))))

(defn solution-1
  [inpt]
  (let [nums (map #(Integer/parseInt %) (s/split-lines inpt))]
    (count-increase nums)))

(defn solution-2
  [inpt]
  (let [nums (map #(Integer/parseInt %) (s/split-lines inpt))]
    (count-sliding-increase nums)))
