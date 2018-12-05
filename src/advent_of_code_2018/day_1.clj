(ns advent-of-code-2018.day-1
  (:require [clojure.java.io :as io]
            [advent-of-code-2018.util :as util]))

(defn read-input
  []
  (->> (util/read-file-lines "day-1-input.txt")
       (reduce #(conj %1 (Integer/parseInt %2)) [])))

(defn part-one
  []
  (let [lines (read-input)]
    (reduce #(+ %2 %1) 0 lines)))

(defn part-two
  []
  (let [lines (cycle (read-input))]
    (loop [frequency-iter lines
           duplicate? false
           frequency-set #{}
           frequency 0]
      (if duplicate?
        frequency
        (let [sum (+ frequency (first frequency-iter))]
          (recur (rest frequency-iter)
                   (contains? frequency-set sum)
                   (conj frequency-set sum)
                   sum))))))
