(ns advent-of-code-2018.day-5
  (:require [clojure.java.io :as io]
            [advent-of-code-2018.util :as util]
            [clojure.string :as str]))

(defn read-input
  []
  (-> (io/resource "day-5-input.txt")
      io/reader
      slurp))

(defn react
  [string]
  (loop [i 0
         result-string string]
    (if (= i (dec (count result-string)))
      result-string
      (let [c1 (nth result-string i)
            c2 (nth result-string (inc i))]
        ;; Exact characters don't match, but when coerced to uppercase, they do
        (if (and (not (= c1 c2))
                 (= (str/upper-case c1)
                    (str/upper-case c2)))
          (recur 0
                 (str (subs result-string 0 i) (subs result-string (+ i 2))))
          (recur (inc i)
                 result-string))))))

(defn part-one
  []
  (-> (read-input)
      react
      count
      dec
      (str " units remain after all reactions.")))

(defn catalyse
  [string]
  (let [lowers "abcdefghijklmnopqrstuvwxyz"
        uppers "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
    (loop [i 0
           shortest-string string]
      (if (= i (count uppers))
        shortest-string
        (recur (inc i)
               (let [catalysed-string (-> string
                                          (str/replace (re-pattern (str "[" (nth lowers i) (nth uppers i) "]"))
                                                       "")
                                          react)]
                 (if (< (count catalysed-string) (count shortest-string))
                   catalysed-string
                   shortest-string)))))))

(defn part-two
  []
  (let [polymer (read-input)
        catalysed-polymer (catalyse polymer)]
    (-> (catalysed-polymer)
        count
        dec
        (str " units remain after all the catalysed reactions."))))
