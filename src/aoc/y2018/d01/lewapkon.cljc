(ns aoc.y2018.d01.lewapkon
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as str]))

(def test-data (map u/read-string (str/split-lines input)))

(defn solve-1 [data]
  (apply + data)
)

(defn solve-2 [data]
  (reduce
    (fn [state diff]
      (let [curr (state :curr)
            seen (state :seen)
            new (+ curr diff)]
      (if (seen curr)
        (reduced curr)
        {:curr new :seen (conj! seen curr)})))
    {:curr 0 :seen (transient #{})} (cycle data))
)

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1 test-data)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2 test-data)))))
