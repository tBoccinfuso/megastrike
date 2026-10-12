(ns megastrike.deployment-strategy-test
  (:require [clojure.test :refer [deftest is testing]]
            [megastrike.board :as board]
            [megastrike.deployment :as deployment]
            [megastrike.deployment-strategy :as strategy]))

(defn sample-game []
  {:game-board (board/create-board 8 8)
   :forces [{:unit-group/keyword :blue :unit-group/deployment :direction/n}]
   :units [{:unit/id "front" :unit/battle-force :blue :unit/location {}
            :unit/attacks {:attack/regular {:attack/s 4 :attack/m 2 :attack/l 0}}
            :unit/move-modes {:move/walk 5}}]})

(deftest every-preferred-tile-is-legal
  (let [game (sample-game)
        unit (first (:units game))
        choices (deployment/legal-tiles game unit 3)
        ranked (vec (strategy/rank-legal-tiles game unit choices))]
    (is (= (set (map deployment/location-key choices))
           (set (map deployment/location-key ranked))))
    (is (= (count choices) (count ranked)))
    (is (deployment/eligible-location? game unit (first ranked) 3))))

(deftest deterministic-ranking
  (let [game (sample-game)
        unit (first (:units game))
        candidates (deployment/legal-tiles game unit 3)]
    (is (= (map deployment/location-key (strategy/rank-legal-tiles game unit candidates))
           (map deployment/location-key (strategy/rank-legal-tiles game unit (reverse candidates)))))))

(deftest never-fabricates-a-location
  (let [game (sample-game)
        unit (first (:units game))]
    (is (nil? (strategy/best-legal-tile
                (assoc-in game [:forces 0 :unit-group/deployment] :direction/none)
                unit 3)))))
