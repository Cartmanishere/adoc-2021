(ns adoc-2021.day-9
  (:require [aocd.core :as data]
            [clojure.string :as s]))

(def input (delay (data/input 2021 9)))

;; ──────────────────────────── Load data ───────────────────────────

(defn load-data
  [inpt]
  (->> inpt
       s/split-lines
       (mapv (fn [line]
              (mapv #(Integer/parseInt %)
                   (s/split line #""))))))

;; ────────────────────────── For solution 1 ──────────────────────────

(defn low-point?
  [floor [x y] rown coln]
  (let [h (get-in floor [x y])]
    (cond-> true
      (< (inc x) rown) (#(and %
                              (< h (get-in floor [(inc x) y]))))
      (>= (dec x) 0) (#(and %
                            (< h (get-in floor [(dec x) y]))))
      (< (inc y) coln) (#(and %
                              (< h (get-in floor [x (inc y)]))))
      (>= (dec y) 0) (#(and %
                            (< h (get-in floor [x (dec y)])))))))

(defn get-low-points
  [floor]
  (let [rown (count floor)
        coln (count (first floor))]
    (filter #(low-point? floor % rown coln)
            (for [x (range rown)
                  y (range coln)]
              [x y]))))

(defn calc-risk
  [floor]
  (reduce (fn [risk pt]
            (+ risk (inc (get-in floor pt))))
          0
          (get-low-points floor)))

(defn soln-1
  []
  (-> @input
      load-data
      calc-risk))

;; ────────────────────────── For solution 2 ──────────────────────────

(def visited-map (atom #{}))

(defn visited?
  [pt]
  (boolean (some @visited-map #{pt})))

(defn visit
  [pt]
  (swap! visited-map conj pt))


(defn explore-basin
  "Returns basin size."
  [counter floor [x y] rown coln]
  (if (or (= 9 (get-in floor [x y]))
          (visited? [x y]))
    counter
    (do (visit [x y])
        (inc
         (cond-> counter
           (< (inc x) rown) (explore-basin floor [(inc x) y] rown coln)
           (>= (dec x) 0) (explore-basin floor [(dec x) y] rown coln)
           (< (inc y) coln) (explore-basin floor [x (inc y)] rown coln)
           (>= (dec y) 0) (explore-basin floor [x (dec y)] rown coln))))))

(defn calc-basins
  [floor]
  (let [low-pts (get-low-points floor)
        rown (count floor)
        coln (count (first floor))]
    (->> low-pts
         (map (fn [pt]
                (swap! visited-map (constantly #{}))
                (explore-basin 0
                              floor
                              pt
                              rown
                              coln)))
         (sort >)
         (take 3)
         (apply *))))

(defn soln-2
  []
  (-> @input
      load-data
      calc-basins))
