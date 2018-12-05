(ns advent-of-code-2018.day-3
  (:require [advent-of-code-2018.util :as util]
            [clojure.string :as str]))

(defn parse-line
  [line]
  (let [elements (str/split line #"[ :]")]
    {:id (first elements)
     :coords (map #(Integer/parseInt %) (str/split (nth elements 2) #","))
     :area (map #(Integer/parseInt %) (str/split (nth elements 4) #"x"))}))
 
(defn read-input
  []
  (->> (util/read-file-lines "day-3-input.txt")
       (reduce #(conj %1 (parse-line %2)) [])))

(defn within?
  [min-n max-n n-1 n-2]
  (or
   (and (>= n-1 min-n) (<= n-1 max-n))
   (and (>= n-2 min-n) (<= n-2 max-n))
   (and (< n-1 min-n) (> n-2 max-n))))

(defn square-meta
  [square]
  (let [min-x (first (:coords square))
        max-x (+ min-x (dec (first (:area square))))
        min-y (nth (:coords square) 1)
        max-y (+ min-y (dec (nth (:area square) 1)))]
    {:min-x min-x :max-x max-x :min-y min-y :max-y max-y}))

(defn squares-overlap?
  [square-1 square-2]
  (and
    (within? (:min-x square-1) (:max-x square-1) (:min-x square-2) (:max-x square-2))
    (within? (:min-y square-1) (:max-y square-1) (:min-y square-2) (:max-y square-2))))

(defn calculate-overlap
  [square-1 square-2]
  (if (squares-overlap? square-1 square-2)
    (let [start-x (max (:min-x square-1) (:min-x square-2))
          end-x (min (:max-x square-1) (:max-x square-2))
          length-x (inc (- end-x start-x))
          start-y (max (:min-y square-1) (:min-y square-2))
          end-y (min (:max-y square-1) (:max-y square-2))
          length-y (inc (- end-y start-y))]
        (loop [y start-y
               result []]
          (if (> y end-y)
            result
            (recur (inc y)
                   (into result (loop [x start-x
                                       sub-result []]
                                  (if (> x end-x)
                                    sub-result
                                    (recur (inc x)
                                           (conj sub-result (str x "," y))))))))))))

(defn overlaps
  [squares]
  (let [overlap-matrix (repeat 1000 (repeat 1000 0))]
    (loop [square-iter squares
           result #{}]
      (if (empty? square-iter)
        result
        (recur (rest square-iter)
               (let [square-1 (square-meta (first square-iter))]
                 (loop [test-iter (rest square-iter)
                        test-result #{}]
                   (if (empty? test-iter)
                     (reduce #(conj %1 %2) result test-result)
                     (recur (rest test-iter)
                            (let [square-2 (square-meta (first test-iter))]
                              (reduce #(conj %1 %2) test-result (calculate-overlap square-1 square-2))))))))))))

(defn part-one
  []
  (count (overlaps (read-input))))

(defn no-overlap?
  [square overlap-set]
  (let [start-x (first (:coords square))
        end-x (+ start-x (dec (first (:area square))))
        start-y (nth (:coords square) 1)
        end-y (+ start-y (dec (nth (:area square) 1)))]
  (loop [y start-y
         overlap? false]
    (if (or overlap? (> y end-y))
      (not overlap?)
      (recur (inc y)
             (loop [x start-x
                    found? false]
               (if (or found? (> x end-x))
                 found?
                 (recur (inc x)
                        (let [coord (str x "," y)]
                          (contains? overlap-set coord))))))))))

(defn part-two
  []
  (let [squares (read-input)
        overlap-set (overlaps squares)]
    (loop [square-iter squares
           id nil]
      (if (or (not (nil? id)) (empty? square-iter))
        id
        (recur (rest square-iter)
               (if (no-overlap? (first square-iter) overlap-set)
                 (prn (first square-iter))))))))
