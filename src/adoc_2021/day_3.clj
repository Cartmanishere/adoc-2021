(ns adoc-2021.day-3
  (:require [aocd.core :as data]
            [clojure.string :as s]))

(def input (delay (data/input 2021 3)))

(defn get-nums
  [inpt]
  (->> (s/split-lines inpt)
       (map seq)
       (map (fn [x] (map #(Character/getNumericValue %) x)))))

(defn bin->dec
  "Convert a binary array of bits into its decimal number."
  [bin]
  (Integer/parseInt (s/join bin) 2))

(defn inverse
  "Given a binary bit array, inverse the values."
  [bin]
  (map #(bit-flip % 0) bin))

(defn reduce-nums
  "Add a list of binary bit arrays."
  [nums]
  (reduce (fn [acc n]
            (map + acc n))
          nums))

;; ────────────────────────── For solution 1 ──────────────────────────

(defn calc-rates
  "Return gamma and epsilon rates."
  [nums]
  (let [sum (reduce-nums nums)
        half (/ (count nums) 2)
        bin (map #(if (> % half) 1 0) sum)]
    {:gamma (bin->dec bin)
     :epsilon (bin->dec (inverse bin))}))

(defn soln-1
  []
  (let [nums (get-nums @input)
        {:keys [gamma epsilon]} (calc-rates nums)]
    (* gamma epsilon)))

;; ────────────────────────── For solution 2 ──────────────────────────

(defn filter-nums
  [cmp nums idx]
  (let [sum (reduce-nums nums)
        half (/ (count nums) 2)
        chooser-bit (if (cmp (nth sum idx) half) 1 0)]
    (filter #(= chooser-bit (nth % idx)) nums)))

(defn calc-rating
  [cmp nums]
  (loop [idx 0
         nums nums]
    (if (= 1 (count nums))
      (first nums)
      (recur (inc idx)
             (filter-nums cmp nums idx)))))

(defn oxygen-gen-rating
  [nums]
  (bin->dec (calc-rating >= nums)))

(defn co2-scrubber-rating
  [nums]
  (bin->dec (calc-rating < nums)))

(defn life-support-rating
  [nums]
  (* (oxygen-gen-rating nums)
     (co2-scrubber-rating nums)))

(defn soln-2
  []
  (life-support-rating (get-nums @input)))
