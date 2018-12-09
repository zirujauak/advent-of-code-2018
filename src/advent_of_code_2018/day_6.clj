(ns advent-of-code-2018.day-6
  (:require [advent-of-code-2018.util :as util]
            [clojure.string :as s]))

(defn parse-line
  [line]
  (let [coords (s/split line #", ")]
    {:x (Integer/parseInt (nth coords 0))
     :y (Integer/parseInt (nth coords 1))}))

(defn read-input
  []
  (->> (util/read-file-as-lines 6)
       (reduce #(conj %1 (parse-line %2)) #{})))

(defn max-coord
  "Find the further x and y values"
  [coords key]
  (loop [coord-iter coords
         max-n 0]
    (if (empty? coord-iter)
      max-n
      (recur (rest coord-iter)
             (let [n (get (first coord-iter) key)]
               (if (> n max-n)
                 n
                 max-n))))))

(defn calculate-distance
  "Calculate the distance between two coordinates"
  [coord-1 coord-2]
  (let [d-x (Math/abs (- (:x coord-1) (:x coord-2)))
        d-y (Math/abs (- (:y coord-1) (:y coord-2)))]
    (+ d-x d-y)))

(defn calculate-coord-distances
  "Calculate the distance from a given coordinate to the set of coordinates. If a distance of 0 is found then 
the calculation is cut off."
  [coord-x coord-y coord-set]
  (let [coord-prime {:x coord-x :y coord-y}]
    (loop [coord-iter coord-set
           distance-map {}
           abort false]
      (if (empty? coord-iter)
        (into (sorted-map) distance-map)
        (recur (rest coord-iter)
               (let [coord (first coord-iter)
                     distance (calculate-distance coord-prime coord)]
                 (assoc distance-map distance
                        (conj (get distance-map distance) (str (:x coord) "," (:y coord)))))
               (contains? distance-map 0))))))

(defn check-owner
  "If there is a unique owner for a coordinate, add it to the set"
  [coord-set distance-seq]
  (if (= (count distance-seq) 1)
    (let [coord-string (first distance-seq)
          coord-elems (s/split coord-string #",")
          coord {:x (Integer/parseInt (nth coord-elems 0)) :y (Integer/parseInt (nth coord-elems 1))}]
      (conj coord-set coord))
    coord-set))

(defn find-infinite-coords
  "Find the set of coordinates with infinite area - any coordinate that owns an edge point is effectively
infinite."
  [coord-set]
  (let [max-x (max-coord coord-set :x)
        max-y (max-coord coord-set :y)]
    (loop [x 0
           y 0
           infinite-coords #{}]
      (if (and (> x max-x) (> y max-y))
        infinite-coords
        (recur (if (<= x max-x) (inc x) x)
               (if (<= y max-y) (inc y) y)
               (let [distance-map-1 (calculate-coord-distances x 0 coord-set)
                     distance-map-2 (calculate-coord-distances 0 y coord-set)
                     distance-map-3 (calculate-coord-distances x max-y coord-set)
                     distance-map-4 (calculate-coord-distances max-x y coord-set)
                     closest-coords-1 (val (first distance-map-1))
                     closest-coords-2 (val (first distance-map-2))
                     closest-coords-3 (val (first distance-map-3))
                     closest-coords-4 (val (first distance-map-4))]
                 (-> infinite-coords
                     (check-owner closest-coords-1)
                     (check-owner closest-coords-2)
                     (check-owner closest-coords-3)
                     (check-owner closest-coords-4))))))))

(defn find-owner-for-point
  "Determine which coordinate, if any, owns a point."
  [x y coord-set]
  (let [coord-point {:x x :y y}]
    (loop [coord-iter coord-set
           min-distance 9999999
           closest-coord nil]
      (if (empty? coord-iter)
        closest-coord
        (let [coord (first coord-iter)
              distance (calculate-distance coord-point coord)]
          (cond
            (< distance min-distance)
            (recur (rest coord-iter)
                   distance
                   coord)

            (= distance min-distance)
            (recur (rest coord-iter)
                   distance
                   nil)

            :else
            (recur (rest coord-iter)
                   min-distance
                   closest-coord)))))))

(defn find-owned-point-count
  "Find the count of points owned by each coordinate."
  [coord-set]
  (let [max-x (max-coord coord-set :x)
        max-y (max-coord coord-set :y)]
    (loop [x 0
           y 0
           owner-map {}]
      (if (= x max-x)
        owner-map
        (if (= y max-y)
          (recur (inc x) 0 owner-map)
          (let [owner (find-owner-for-point x y coord-set)
                owner-key (if (nil? owner) nil (str (:x owner) "," (:y owner)))
                count (get owner-map owner-key 0)
                update-map (if (nil? owner) owner-map (assoc owner-map owner-key (inc count)))]
            (recur x (inc y) update-map)))))))

(defn find-largest-points
  "Create a map of coordinates and owned points, sorted by the number of points owned in descending order."
  [coord-set owner-map]
  (let [infinite-coords (find-infinite-coords coord-set)
        owner-map (find-owned-point-count coord-set)
        candidate-map (reduce #(dissoc %1 (str (get %2 :x) "," (get %2 :y))) owner-map infinite-coords)]
    (into (sorted-map-by (fn [k1 k2]
                           (compare [(get candidate-map k2) k2]
                                    [(get candidate-map k1) k1])))
          candidate-map)))

(defn part-one
  "Find the coordinate that is closest to the largest number of points on the grid."
  []
  (let [data (read-input)
        result-map (find-largest-points data (find-owned-points data))]
    (str "The largest owned area is " (val (first result-map)))))

(defn calculate-total-distance
  "Calculate the sum of the distances between a point and every coordinate."
  [x y coord-set]
  (let [subject-coord {:x x :y y}]
    (loop [coord-iter coord-set
           distance 0]
      (if (empty? coord-iter)
        distance
        (recur (rest coord-iter)
               (+ distance (calculate-distance subject-coord (first coord-iter))))))))

(defn calculate-region
  "Calculate the number of points where the sum of distances between the point and every coordinate is less
than max-distance."
  [max-distance coord-set]
  (let [max-x (max-coord coord-set :x)
        max-y (max-coord coord-set :y)]
    (loop [x 0
           y 0
           region-set #{}]
      (if (> x max-x)
        region-set
        (if (> y max-y)
          (recur (inc x) 0 region-set)
          (recur x (inc y)
                 (let [distance (calculate-total-distance x y coord-set)]
                   (if (< distance max-distance)
                     (conj region-set {:x x :y y})
                     region-set))))))))

(defn part-two
  "Calculate the size of the region where the sum distance between each point and every coordinate is less than
10,000."
  []
  (let [region (calculate-region 10000 (read-input))]
    (str "The 'safe' region size is " (count region))))
