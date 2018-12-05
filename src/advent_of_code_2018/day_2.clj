(ns advent-of-code-2018.day-2
  (:require [clojure.java.io :as io]
            [advent-of-code-2018.util :as util]))


(defn read-input
  []
  (util/read-file-lines "day-2-input.txt"))

(defn check-letter-count
  [string target-count]
  (loop [letter-iter string
         matched? false]
    (if (or matched? (empty? letter-iter))
      matched?
      (recur (rest letter-iter)
             (= target-count (count (filter #(= (str %) (str (first letter-iter))) string)))))))

(defn part-one
  []
  (let [lines (read-input)]
    (loop [line-iter lines
           two-letter 0
           three-letter 0]
      (if (empty? line-iter)
        (* two-letter three-letter)
        (recur (rest line-iter)
               (if (check-letter-count (first line-iter) 2) (inc two-letter) two-letter)
               (if (check-letter-count (first line-iter) 3) (inc three-letter) three-letter))))))

(defn check-differences
  [string-1 string-2]
  (loop [string-1-iter string-1
         string-2-iter string-2
         diff-pos []
         index 0]
    (if (empty? string-1-iter)
      diff-pos
      (recur (rest string-1-iter)
             (rest string-2-iter)
             (if (= (first string-1-iter) (first string-2-iter))
               diff-pos
               (conj diff-pos index))
             (inc index)))))

(defn part-two
  []
  (let [lines (read-input)]
    (loop [line-iter lines
           result nil]
      (if (or (not (nil? result)) (empty? line-iter))
        result
        (recur (rest line-iter)
               (let [string-a (first line-iter)]
                 (loop [comp-iter (rest line-iter)
                        comp-result nil]
                   (if (or (not (nil? comp-result)) (empty? comp-iter))
                     comp-result
                     (recur (rest comp-iter)
                       (let [string-b (first comp-iter)
                             diff-pos (check-differences string-a string-b)]
                         (if (= (count diff-pos) 1)
                           (str (subs string-a 0 (first diff-pos)) (subs string-a (inc (first diff-pos)))))))))))))))

