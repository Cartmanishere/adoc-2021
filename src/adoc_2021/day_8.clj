(ns adoc-2021.day-8
  (:require [aocd.core :as data]
            [clojure.string :as s]))

(def input (delay (data/input 2021 8)))

;; ────────────────────────── For solution 1 ──────────────────────────

;; Quick dirty solution

(defn get-outputs
  [inpt]
  (->> inpt
       s/split-lines
       (mapcat (comp #(s/split % #" ")
                  s/trim
                  second
                  #(s/split % #"\|")))))

(def unique #{2 4 3 7})

(defn soln-1
  []
  (->> @input
       get-outputs
       (map count)
       (filter #(some unique #{%}))
       count))

;; ────────────────────────── For solution 2 ──────────────────────────

;; Brute force solution:
;; Consider all possible permutations and try to find which permutation
;; matches all the numbers in the input.

(defn permutations
  "Return all possible permutations of a collection.
  Copied from: https://stackoverflow.com/a/26076145"
  [s]
  (lazy-seq (if (seq (rest s))
              (apply concat
                     (for [x s]
                       (map #(cons x %)
                            (permutations (remove #{x} s)))))
              [s])))

(def n-map
  {#{0 1 2 4 5 6} 0
   #{2 5} 1
   #{0 2 3 4 6} 2
   #{0 2 3 5 6} 3
   #{1 2 3 5} 4
   #{0 1 3 5 6} 5
   #{0 1 3 4 5 6} 6
   #{0 2 5} 7
   #{0 1 2 3 4 5 6} 8
   #{0 1 2 3 5 6} 9})

(defn get-perm-map
  [perm]
  (into {} (map (fn [i] [(get perm i) i]) (range (count perm)))))

(defn parse-line
  [line]
  (let [parts (map (fn [part]
                     (s/split (s/trim part) #" "))
                   (s/split line #"\|"))]
    {:codes (first parts)
     :output (second parts)}))

(defn load-data
  [inpt]
  (->> inpt
       s/split-lines
       (map parse-line)))

(defn num->str
  "Given a particular permutation, get the string representation of that num."
  [perm n]
  (reduce (fn [s idx]
            (str s (get perm idx)))
          ""
          (get n-map n)))

(defn str->num
  "Given a particular permutation and a input str check which num it is."
  [perm-map s]
  (n-map (set (map (fn [c]
                    (get perm-map (str c)))
                  s))))


(defn valid-perm?
  "Check if a permutation is a valid one."
  [perm-map codes]
  (= 0 (count (filter nil? (map #(str->num perm-map %) codes)))))

(defn find-arragement
  [codes]
  (let [perms (permutations ["a" "b" "c" "d" "e" "f" "g"])]
    (loop [rem (rest perms)
           cur (first perms)]
      (let [perm-map (get-perm-map (vec cur))]
        (if (or (valid-perm? perm-map codes)
                (zero? (count rem)))
          perm-map
          (recur (rest rem)
                 (first rem)))))))

(defn decode-output
  [res-map outputs]
  (Integer/parseInt (s/join (map #(str->num res-map %) outputs))))

(defn soln-2
  []
  (reduce (fn [res {:keys [output codes]}]
            (let [res-map (find-arragement codes)
                  n (decode-output res-map output)]
              (+ res n)))
          0
          (load-data @input)))
