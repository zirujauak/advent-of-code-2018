(ns advent-of-code-2018.day-2
  (:require [advent-of-code-2018.util :as util]
            [clojure.data :as data]
            [clojure.string :as s]))

(defn has-letter-n-times
  [line n]
  (if (empty? (filter #(= n (val %)) (frequencies line)))
    0
    1))

(defn part-one
  []
  (let [lines (util/read-file-as-lines 2)]
    (loop [line-iter lines
           two-letter 0
           three-letter 0]
      (if (empty? line-iter)
        (* two-letter three-letter)
        (recur (rest line-iter)
               (+ two-letter (has-letter-n-times (first line-iter) 2))
               (+ three-letter (has-letter-n-times (first line-iter) 3)))))))

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

(defn check-for-n-differences
  [string-1 string-2 n]
  (let [diff (data/diff (seq string-1) (seq string-2))
        diff-count (count (filter #(not (nil? %)) (first diff)))]
    (if (= n diff-count)
      (s/join (nth diff 2)))))

(defn compare-string-with-sequence
  [string remaining-seq]
  (loop [comp-iter remaining-seq]
    (if (empty? comp-iter)
      nil
      (let [string (check-for-n-differences string (first comp-iter) 1)]
        (if-not (nil? string)
          string
          (recur (rest comp-iter)))))))

(defn part-two
  []
  (let [lines (sort (util/read-file-as-lines 2))]
    (loop [line-iter lines]
      (if (empty? line-iter)
        nil
        (let [common-string (compare-string-with-sequence (first line-iter) (rest line-iter))]
          (if (not (nil? common-string))
            common-string
            (recur (rest line-iter))))))))

