(ns advent-of-code-2018.day-4
  (:require [advent-of-code-2018.util :as util]
            [clojure.string :as str]))

(defn event->type 
  [event]
  (cond (= "wakes up" event) :wakes-up
        (= "falls asleep" event) :falls-asleep
        (re-matches #"Guard #\d+ begins shift" event) :shift-change))

(defn event->guard-id
  [event]
  (nth (re-find #"Guard #(\d+) begins shift" event) 1))

(defn parse-line
  [line]
  (let [timestamp (subs line 1 17)
        date (subs line 1 11)
        minute (Integer/parseInt (subs line 15 17))
        event (subs line 19)
        event-type (event->type event)
        guard-id (event->guard-id event)]
    {:event event
     :timestamp timestamp
     :date date
     :minute minute
     :event-type event-type
     :guard-id guard-id}))

(defn read-input
  []
  (->> (util/read-file-lines "day-4-input.txt")
       (reduce #(conj %1 (parse-line %2)) [])
       (sort #(compare (:timestamp %1) (:timestamp %2)))))

(defonce current-event-type (atom nil))
(defonce current-guard-id (atom nil))
(defonce current-start-minute (atom nil))

(defn add-sleep-minutes
  [event sleep-map]
  (let [end-minute (:minute event)]
    (loop [minute @current-start-minute
           minute-map (get sleep-map @current-guard-id)]
      (if (= minute end-minute)
        (assoc sleep-map @current-guard-id minute-map)
        (recur (if (= minute 59) 0 (inc minute))
               (let [minute-seq (get minute-map minute)]
                 (assoc minute-map minute (conj minute-seq (:date event)))))))))

(defn shift-change
  [event sleep-map]
  (let [result (if (= @current-event-type :falls-asleep)
                 (add-sleep-minutes event sleep-map)
                 sleep-map)]
    (reset! current-event-type :shift-change)
    (reset! current-guard-id (:guard-id event))
    (reset! current-start-minute (:minute event))
    result))

(defn falls-asleep
  [event sleep-map]
  (reset! current-event-type :falls-asleep)
  (reset! current-start-minute (:minute event))
  sleep-map)

(defn wakes-up
  [event sleep-map]
  (let [result (if (= @current-event-type :falls-asleep)
                 (add-sleep-minutes event sleep-map)
                 sleep-map)]
    (reset! current-event-type :wakes-up)
    (reset! current-start-minute (:minute event))
    result))

(defn process-event
  [event sleep-map]
  (case (:event-type event)
    :shift-change (shift-change event sleep-map)
    :falls-asleep (falls-asleep event sleep-map)
    :wakes-up (wakes-up event sleep-map)))

(defn process-events
  [event-seq]
  (reset! current-event-type nil)
  (reset! current-guard-id nil)
  (reset! current-start-minute nil)
  (let [sleep-map (loop [event-iter event-seq
                         process-map {}]
                    (if (empty? event-iter)
                      process-map
                      (recur (rest event-iter)
                             (process-event (first event-iter) process-map))))]
    (if (= @current-event-type :falls-asleep)
      (process-event {:minute 59 :date (:date (last event-seq)) :event-type :wakes-up} sleep-map)
      sleep-map)))

(def sleepiest-guard-id (atom 0))
(def sleepiest-minute-count (atom 0))
(def sleepiest-minute (atom nil))

(defn find-sleepiest-guard
  [sleep-map]
  (loop [sleep-iter sleep-map]
    (if-not (empty? sleep-iter)
      (let [entry (first sleep-iter)
            guard-id (key entry)
            minute-map (val entry)
            minute-count (reduce #(+ %1 (count (val %2))) 0 minute-map)
            minute (key (first (sort-by #(count (val %)) > minute-map)))]
        (if (> minute-count @sleepiest-minute-count)
          (do
            (reset! sleepiest-guard-id guard-id)
            (reset! sleepiest-minute-count minute-count)
            (reset! sleepiest-minute minute)))
        (recur (rest sleep-iter))))))

(defn part-one
  []
  (-> (read-input)
      process-events
      find-sleepiest-guard)
  (str "Guard #" @sleepiest-guard-id " slept a total of " @sleepiest-minute-count
       " minutes and was most often asleep during minute " @sleepiest-minute))

(defn find-sleepiest-minute
  [sleep-map]
  (loop [sleep-iter sleep-map]
    (if-not (empty? sleep-iter)
      (let [entry (first sleep-iter)
            guard-id (key entry)
            minute-map (val entry)
            minute-entry (first (sort-by #(count (val %)) > minute-map))
            minute (key minute-entry)
            minute-count (count (val minute-entry))]
        (if (> minute-count @sleepiest-minute-count)
          (do
            (reset! sleepiest-guard-id guard-id)
            (reset! sleepiest-minute-count minute-count)
            (reset! sleepiest-minute minute)))
        (recur (rest sleep-iter))))))

(defn part-two
  []
  (-> (read-input)
      process-events
      find-sleepiest-minute)
  (str "Guard #" @sleepiest-guard-id " was asleep " @sleepiest-minute-count " times during minute "
       @sleepiest-minute))
