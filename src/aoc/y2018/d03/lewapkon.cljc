(ns aoc.y2018.d03.lewapkon
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is]]
   [clojure.string :as str]
   [clojure.data :refer [diff]]
   [clojure.set :refer [union]]))

(def pattern #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(def test-data
  (->> input
    str/split-lines
    (map #(re-find pattern %))
    (map rest)
    (map #(map u/read-string %))))

(defn get-all-points [data]
  (mapcat
    (fn [[id x1 y1 w h]]
      (let [x2 (+ x1 w)
            y2 (+ y1 h)]
        (for [x (range x1 x2)
              y (range y1 y2)]
          [id x y])))
    data))

(defn solve-1 [data]
  (->> data
    get-all-points
    (map rest)
    frequencies
    (filter (fn [[coords freq]] (> freq 1)))
    count)
)

(defn points-to-ids [points]
  (persistent!
    (reduce (fn [state [id x y]]
              (let [ids (get state [x y] #{})]
                (assoc! state [x y] (conj ids id))))
            (transient {}) points)))

(defn solve-2 [data]
  (let [ids (set (map first data))]
    (->> data
      get-all-points
      points-to-ids
      vals
      (filter #(> (count %) 1))
      (apply union)
      (diff ids)
      first
      first))
)

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1 test-data)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2 test-data)))))
