(ns adoc-2021.day-5
  (:require [aocd.core :as data]
            [clojure.string :as s])
  (:import [java.lang Math]))

(def input (delay (data/input 2021 5)))

;; ──────────────────────────── Load data ───────────────────────────

(defn parse-row
  "Parse a line input into x1, y1, x2, y2."
  [line]
  (->> (s/split line #"->")
       (map s/trim)
       (map (fn [x]
              (map #(Integer/parseInt %)
                   (s/split x #","))))))

(defn load-data
  [inpt]
  (map parse-row (s/split-lines inpt)))

;; ────────────────────────────── Common ──────────────────────────────

(defn diagonal?
  "Check if the line is diagonal."
  [[[x1 y1] [x2 y2]]]
  (try
    (= 1 (Math/abs (/ (- y2 y1) (- x2 x1))))
    (catch ArithmeticException _
      false)))

(defn straight?
  "Check if a line is vertical or horizontal."
  [[[x1 y1] [x2 y2]]]
  (or (= x1 x2)
      (= y1 y2)))

(defn walk
  [start end]
  (if (> start end)
    (range start (dec end) -1)
    (range start (inc end))))

(defn- gen-pts-diagonal
  "Generate intermediate points for the diagonal lines."
  [[[x1 y1] [x2 y2]]]
  (map vector
       (walk x1 x2)
       (walk y1 y2)))

(defn- gen-pts
  "Given two points of a line, generate all the intermediate points."
  [pts]
  (let [[[x1 y1] [x2 y2]] pts]
    (cond
      ;; Vertical line
      (= x1 x2) (map #(conj [x1] %) (walk y1 y2))

      ;; Horizontal line
      (= y1 y2) (map #(conj [%] y1) (walk x1 x2))

      ;; Diagonal line
      (diagonal? pts) (gen-pts-diagonal pts))))

(defn count-pts
  [lines pred-fn]
  (->> lines
       (filter pred-fn)
       (mapcat gen-pts)
       frequencies
       vals
       (filter (partial < 1))
       count))

;; ────────────────────────── For solution 1 ──────────────────────────

(defn soln-1
  [inpt]
  (count-pts (load-data inpt) straight?))

;; ────────────────────────── For solution 2 ──────────────────────────

(defn soln-2
  [inpt]
  (count-pts (load-data inpt)
             (some-fn straight? diagonal?)))
