(ns advent-of-code-2018.day-8
  (:require [advent-of-code-2018.util :as util]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(defn read-input
  []
  (reduce #(conj %1 (Integer/parseInt %2)) []
          (-> (io/resource "day-8-input.txt")
              io/reader
              slurp
              (s/split #"\s"))))

(def file-position (atom 0))
(def checksum (atom 0))

(defn fp
  []
  (let [result @file-position]
    (swap! file-position inc)
    result))

(defn read-metadata
  [file-seq n]
  (let [result (take n (nthrest file-seq @file-position))]
    (swap! checksum + (reduce + result))
    (reset! file-position (+ n @file-position))
    result))

(defn parse-node
  [file-seq]
  (let [n-children (nth file-seq (fp))
        n-metadata (nth file-seq (fp))
        children (loop [i 0
                        child-seq []]
                   (if (= i n-children)
                     child-seq
                     (recur (inc i)
                            (conj child-seq (parse-node file-seq)))))
        metadata (read-metadata file-seq n-metadata)]
    {:n-children n-children :children children :n-metadata n-metadata :metadata metadata}))

(defn part-one
  []
  (reset! file-position 0)
  (reset! checksum 0)
  (let [file-seq (read-input)]
    (parse-node file-seq)
    @checksum))

