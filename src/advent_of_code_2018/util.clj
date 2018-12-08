(ns advent-of-code-2018.util
  (:require [clojure.java.io :as io]))

(defn input-filename
  [day]
  (str "day-" day "-input.txt"))

(defn read-file
  [day]
  (-> day
      input-filename
      io/resource
      io/reader
      slurp))

(defn read-file-as-lines
  [day]
  (-> day
      input-filename
      io/resource
      io/reader
      line-seq))
