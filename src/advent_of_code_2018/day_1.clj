(ns advent-of-code-2018.day-1
  (:require [advent-of-code-2018.util :as util]))

(defn read-input
  []
  (->> 1
       util/read-file-as-lines
       (reduce #(conj %1 (Integer/parseInt %2)) [])))

(defn part-one
  []
  (let [frequency-seq (read-input)]
    (reduce + frequency-seq)))

(defn part-two
  []
  (let [lines (cycle (read-input))]
    (loop [frequency-iter lines
           frequency-set #{}
           frequency-sum 0]
        (let [sum (+ frequency-sum (first frequency-iter))]
          (if (contains? frequency-set sum)
            sum
            (recur (rest frequency-iter)
                   (conj frequency-set sum)
                   sum))))))
