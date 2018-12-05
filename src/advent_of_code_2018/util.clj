(ns advent-of-code-2018.util
  (:require [clojure.java.io :as io]))

(defn read-file-lines
  [filename]
  (->> (io/resource filename)
       io/reader
       line-seq))
