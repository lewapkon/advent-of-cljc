(ns aoc.y2018.d02.lewapkon
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as str]
   [clojure.set :refer [map-invert]]))

(def test-data (str/split-lines input))

(defn transpose [matrix] (apply map vector matrix))

(defn count-truthy [arr] (count (filter identity arr)))

(defn solve-1 [data]
  (->> data
    (map frequencies)
    (map map-invert)
    (map #(vector (contains? % 2) (contains? % 3)))
    transpose
    (map count-truthy)
    (apply *)))

(defn common-part [word1 word2]
  (->> (map #(when (= %1 %2) %1) word1 word2) (apply str)))

(defn solve-2 [data]
  (first (for [word1 data
               word2 data
               :let [common (common-part word1 word2)]
               :when (= (count common) (dec (count word1)))]
            common)))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1 test-data)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2 test-data)))))
