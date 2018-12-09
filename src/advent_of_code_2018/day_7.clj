(ns advent-of-code-2018.day-7
  (:require [advent-of-code-2018.util :as util]
            [clojure.string :as s]))

(defn parse-line
  [line]
  (let [elements (first (re-seq #"Step ([A-Z]) must be finished before step ([A-Z]) can begin\." line))]
    {:step (nth elements 2)
     :required-step (nth elements 1)}))

(defn read-input
  []
  (->> (util/read-file-as-lines 7)
       (reduce #(conj %1 (parse-line %2)) [])))

(defn map-dependencies
  [step-seq]
  (loop [step-iter step-seq
         dependency-map {}]
    (if (empty? step-iter)
      dependency-map
      (recur (rest step-iter)
             (let [step (first step-iter)
                   step-key (:step step)
                   dep-seq (get dependency-map step-key #{})]
               (assoc dependency-map step-key (conj dep-seq (:required-step step))))))))

(defn fill-in-ready-steps
  [dependency-map]
  (loop [char-iter "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
         result dependency-map]
    (if (empty? char-iter)
      result
      (recur (rest char-iter)
             (let [step (str (first char-iter))]
               (if-not (contains? result step)
                 (assoc result step #{})
                 result))))))

(defn find-next-step
  [dependency-map]
  (let [ready-steps (->> dependency-map
                         (filter #(empty? (val %)))
                         (into (sorted-map)))]
    (key (first ready-steps))))

(defn clear-dependencies
  [dependency-map step]
  (loop [dep-iter (dissoc dependency-map step)
         new-map {}]
    (if (empty? dep-iter)
      new-map
      (recur (rest dep-iter)
             (let [dep (first dep-iter)
                   dep-step (key dep)
                   dep-seq (val dep)]
               (assoc new-map dep-step (disj dep-seq step)))))))

(defn execute
  [dependency-map]
  (loop [dep-map dependency-map
         result ""]
    (if (empty? dep-map)
      result
      (let [next-step (find-next-step dep-map)
            new-map (clear-dependencies dep-map next-step)]
        (recur new-map (str result next-step))))))


(defn part-one
  []
  (-> (read-input)
      map-dependencies
      fill-in-ready-steps
      execute))

(defn find-ready-steps
  [dependency-map]
  (let [ready-steps (->> dependency-map
                         (filter #(empty? (val %)))
                         (into (sorted-map)))]
    (keys ready-steps)))

(def clock (atom 0))
(def worker-1 (atom {:id 1 :step nil :finish 0}))
(def worker-2 (atom {:id 2 :step nil :finish 0}))
(def worker-3 (atom {:id 3 :step nil :finish 0}))
(def worker-4 (atom {:id 4 :step nil :finish 0}))
(def worker-5 (atom {:id 5 :step nil :finish 0}))

(defn idle-workers
  []
  (filter #(nil? (:step (deref %))) [worker-1 worker-2 worker-3 worker-4 worker-5]))

(defn calculate-step-time
  [step]
  (let [A (Character/getNumericValue \A)
        N (Character/getNumericValue (first step))]
    (+ 61 (- N A))))

(defn dispatch-workers
  [dependency-map]
  (let [ready-steps (find-ready-steps dependency-map)]
    (loop [step-iter ready-steps
           worker-iter (idle-workers)
           new-map dependency-map]
      (if (or (empty? step-iter) (empty? worker-iter))
        new-map
        (recur (rest step-iter)
               (rest worker-iter)
               (let [step (first step-iter)
                     worker (first worker-iter)]
                 (swap! worker assoc :step step :finish (+ @clock (calculate-step-time step)))
                 (dissoc new-map step)))))))

(defn check-worker
  [dependency-map worker]
  (if (= @clock (:finish @worker))
    (do
      (let [step (:step @worker)]
        (swap! worker assoc :step nil)
        (clear-dependencies dependency-map step)))
    dependency-map))

(defn check-workers
  [dependency-map]
  (-> dependency-map
      (check-worker worker-1)
      (check-worker worker-2)
      (check-worker worker-3)
      (check-worker worker-4)
      (check-worker worker-5)))

(defn execute-timed
  [dependency-map]
  (loop [dep-map dependency-map]
    (if (and (empty? dep-map) (= 5 (count (idle-workers))))
      @clock
      (do
        (let [new-map (check-workers dep-map)
              really-new-map (dispatch-workers new-map)]
          (swap! clock inc)
          (recur really-new-map))))))

(defn part-two
  []
  (-> (read-input)
      map-dependencies
      fill-in-ready-steps
      execute-timed
      dec))
