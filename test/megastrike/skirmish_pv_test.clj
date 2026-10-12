(ns megastrike.skirmish-pv-test
  (:require [clojure.test :refer [deftest is testing]]
            [megastrike.skirmish-pv :as pv]))

(deftest preset-limits
  (is (pv/within-bracket? :intro 150))
  (is (pv/within-bracket? :intro 200))
  (is (not (pv/within-bracket? :intro 201)))
  (is (pv/within-bracket? :standard 350))
  (is (not (pv/within-bracket? :standard 249)))
  (is (pv/within-bracket? :large 1000))
  (is (not (pv/within-bracket? :large 399)))
  (is (nil? (pv/validation-message :intro 180 190)))
  (is (string? (pv/validation-message :intro 210 190)))
  (is (string? (pv/validation-message :intro 190 100))))

(deftest filtered-random-force
  (let [catalog [{:unit/type :type/bm :unit/base-pv 30}
                 {:unit/type :type/cv :unit/base-pv 50}]
        roster (pv/random-roster catalog :intro true)]
    (is (seq roster))
    (is (every? #(= :type/bm (:unit/type %)) roster))
    (is (pv/within-bracket? :intro (reduce + (map :unit/base-pv roster))))))
