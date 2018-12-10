(ns advent-of-code-2018.day-9
  (:require [advent-of-code-2018.util :as util]
            [clojure.string :as s]))

(defn read-input
  []
  (let [data (->> (util/read-file 9)
                  (re-seq #"(\d+) players.* (\d+) points")
                  first)]
    {:players (Long/parseLong (nth data 1))
     :marbles (Long/parseLong (nth data 2))}))

(defn next-player
  [state]
  (let [next-player (inc (:current-player state))]
    (assoc state :current-player
           (if (> next-player (:n-players state))
             1
             next-player))))

(defn next-marble
  [state]
  (let [next-marble (inc (:current-marble state))]
    (assoc state :current-marble
           (if (> next-marble (:n-marbles state))
             nil
             next-marble))))

(defn calculate-removal-index
  [state]
  (let [index (- (:marble-index state) 7)]
    (if (< index 0)
      (+ (count (:marbles state)) index)
      index)))

(defn score-marble
  [state]
  (let [player (:current-player state)
        remove-index (calculate-removal-index state)
        score (+ (nth (:marbles state) remove-index) (:current-marble state))
        front (vec (take remove-index (:marbles state)))
        back (nthrest (:marbles state) (inc remove-index))]
    (assoc state :marbles (into front back)
                 :scores (assoc (:scores state) player (+ score (get (:scores state) player 0)))
                 :marble-index remove-index)))

(defn place-marble
  [state]
  (if (= (:current-marble state) 1)
    (assoc state :marbles [0 1]
                 :marble-index 1)
    (if (= (:marble-index state) (dec (count (:marbles state))))
      (assoc state :marbles (into [0 (:current-marble state)] (nthrest (:marbles state) 1))
                   :marble-index 1)
      (let [marble (:current-marble state)
            next-pos (+ (:marble-index state) 2)]
        (if (= next-pos (count (:marbles state)))
          (assoc state :marbles (conj (:marbles state) marble)
                 :marble-index (count (:marbles state)))
          (let [front (vec (take next-pos (:marbles state)))
                back (nthrest (:marbles state) next-pos)]
            (assoc state :marbles (into (conj front marble) back)
                         :marble-index next-pos)))))))

(defn play-marble
  [state]
  (let [new-state (-> state
                      next-player
                      next-marble)]
    (if (nil? (:current-marble new-state))
      new-state
      (if (= 0 (mod (:current-marble new-state) 23))
        (score-marble new-state)
        (place-marble new-state)))))

(defn play
  [state]
  (loop [state state]
    (if (nil? (:current-marble state))
      state
      (recur (play-marble state)))))

(defn create-state
  [n-players n-marbles]
  {:n-players n-players
   :n-marbles n-marbles
   :current-player 0
   :current-marble 0
   :marble-index 0
   :marbles [0]
   :scores {}})

(defn result
  [state]
  (let [winner (first (into (sorted-map-by (fn [k1 k2]
                                             (compare [(get (:scores state) k2) k2]
                                                      [(get (:scores state) k1) k1])))))]
    (str "Elf " (key winner) " won with " (val winner) " points.")))

(defn part-one
  []
  (let [input (read-input)
        state (play (create-state (:players input) (:marbles input)))]
    (result state)))

(defn part-two
  []
  (let [input (read-input)
        state (play (create-state (:players input) (* 100 (:marbles input))))]
    (result state)))
