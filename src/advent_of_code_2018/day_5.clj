(ns advent-of-code-2018.day-5
  (:require [clojure.java.io :as io]
            [advent-of-code-2018.util :as util]
            [clojure.string :as s]))

(defn react
  [string]
  (let [chars "abcdefghijklmnopqrstuvwxyz"]
    (loop [i 0
           result-string string]
      (if (= i (count chars))
        result-string
        (let [reacted-string (-> result-string
                                 (s/replace (re-pattern (str (nth chars i) (s/upper-case (nth chars i)))) "")
                                 (s/replace (re-pattern (str (s/upper-case (nth chars i)) (nth chars i))) ""))]
          (if (< (count reacted-string) (count result-string))
            (recur 0
                   reacted-string)
            (recur (inc i)
                   result-string)))))))

(defn part-one
  []
  (-> (util/read-file 5)
      react
      count
      dec
      (str " units remain after all reactions.")))

(defn catalyse
  [string]
  (let [chars "abcdefghijklmnopqrstuvwxyz"]
    (loop [i 0
           shortest-string string]
      (if (= i (count chars))
        shortest-string
        (recur (inc i)
               (let [catalysed-string (-> string
                                          (s/replace (re-pattern (str "[" (nth chars i) (s/upper-case (nth chars i)) "]"))
                                                     "")
                                          react)]
                 (if (< (count catalysed-string) (count shortest-string))
                   catalysed-string
                   shortest-string)))))))

(defn part-two
  []
  (-> (util/read-input 5)
      catalyse
      count
      dec
      (str " units remain after all the catalysed reactions.")))
