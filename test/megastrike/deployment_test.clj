(ns megastrike.deployment-test
  (:require [clojure.test :refer [deftest is testing]]
            [megastrike.board :as board]
            [megastrike.deployment :as deployment]
            [clojure.set :as set]))

(defn fixture-game []
  {:game-board (board/create-board 6 6)
   :forces [{:unit-group/keyword :blue :unit-group/deployment :direction/n}
            {:unit-group/keyword :red :unit-group/deployment :deployment/s}]
   :units [{:unit/id "blue-1" :unit/battle-force :blue :unit/location {}}
           {:unit/id "red-1" :unit/battle-force :red :unit/location {}}]})

(deftest direction-normalization
  (is (= :n (deployment/normalize-zone :direction/n)))
  (is (= :s (deployment/normalize-zone :deployment/s)))
  (is (nil? (deployment/normalize-zone nil))))

(deftest edge-zones
  (let [game (fixture-game)
        north (deployment/legal-tiles game (first (:units game)) 2)
        south (deployment/legal-tiles game (second (:units game)) 2)]
    (is (= 12 (count north)))
    (is (= 12 (count south)))
    (is (empty? (set/intersection
                  (set (map deployment/location-key north))
                  (set (map deployment/location-key south)))))))

(deftest occupied-tiles-not-offered
  (let [game (fixture-game)
        blue (first (:units game))
        position (first (deployment/legal-tiles game blue 2))
        occupied-game (assoc-in game [:units 1 :unit/location]
                                (deployment/location-key position))]
    (is (deployment/eligible-location? game blue position 2))
    (is (not (deployment/eligible-location? occupied-game blue position 2)))))

(deftest invalid-zones-are-safe
  (let [game (assoc-in (fixture-game) [:forces 0 :unit-group/deployment] :direction/none)]
    (is (empty? (deployment/legal-tiles game (first (:units game)) 2)))))

(deftest cannot-place-outside-board
  (let [game (fixture-game)
        unit (first (:units game))]
    (is (not (deployment/eligible-location? game unit {:hex/p 999 :hex/q 999 :hex/r -1998} 3)))))

(deftest alternate-direction-zones
  (let [game (fixture-game)
        unit (first (:units game))
        east (deployment/zone-tiles (:game-board game) :direction/e 2)]
    (is (= 12 (count east)))
    (is (every? #(some? (:hex/p %)) east))))

(deftest auto-deploy-both-forces
  (let [original (fixture-game)
        {:keys [game unplaced]} (deployment/auto-deploy original 2)
        units (:units game)
        locations (map (comp deployment/location-key :unit/location) units)]
    (is (empty? unplaced))
    (is (every? deployment/deployed? units))
    (is (= (count locations) (count (set locations))))
    (is (every? :unit/acted? units))
    (is (every? (fn [unit]
                  (deployment/eligible-location? original unit (:unit/location unit) 2))
                units))))

(deftest auto-deploy-not-enough-room
  (let [game (-> (fixture-game)
                 (update :units conj {:unit/id "blue-2" :unit/battle-force :blue :unit/location {}})
                 (update :units conj {:unit/id "blue-3" :unit/battle-force :blue :unit/location {}}))
        board (assoc (:game-board game) :tiles (take 1 (:tiles (:game-board game))))
        result (deployment/auto-deploy (assoc game :game-board board) 2)]
    (is (seq (:unplaced result)))
    (is (<= (count (filter deployment/deployed? (get-in result [:game :units]))) 1))))

(deftest stage5b-validation-reasons
  (let [game (fixture-game)
        unit (first (:units game))
        legal (first (deployment/legal-tiles game unit 2))
        outside (first (deployment/legal-tiles game (second (:units game)) 2))]
    (is (nil? (deployment/deployment-reason game unit legal 2)))
    (is (= "Outside your deployment zone."
           (deployment/deployment-reason game unit outside 2)))
    (is (= "That hex is occupied."
           (deployment/deployment-reason (assoc-in game [:units 1 :unit/location]
                                                   (deployment/location-key legal)) unit legal 2)))
    (is (= "Deployment is blocked on that hex."
           (deployment/deployment-reason
             (update-in game [:game-board :tiles]
                        (fn [ts] (mapv #(if (= (deployment/location-key %) (deployment/location-key legal))
                                          (assoc % :deployment/blocked? true) %) ts)))
             unit legal 2)))
    (is (= "Choose a hex on the map."
           (deployment/deployment-reason game unit {:hex/p 900 :hex/q 0 :hex/r -900} 2)))))

(deftest scenario-configured-depth
  (let [game (assoc-in (fixture-game) [:deployment :depth-hexes] 1)
        unit (first (:units game))]
    (is (= 6 (count (deployment/legal-tiles game unit 3))))))

(deftest explicit-scenario-forbidden-terrain
  (let [game (fixture-game)
        unit (first (:units game))
        candidate (first (deployment/legal-tiles game unit 2))
        game (-> game
                 (assoc-in [:deployment :forbidden-terrain] #{"deep_water"})
                 (update-in [:game-board :tiles]
                            (fn [ts] (mapv #(if (= (deployment/location-key %) (deployment/location-key candidate))
                                              (assoc % :terrain "deep_water") %) ts))))]
    (is (= "Deployment is restricted on that terrain."
           (deployment/deployment-reason game unit candidate 2)))
    (is (not (deployment/eligible-location? game unit candidate 2)))))
