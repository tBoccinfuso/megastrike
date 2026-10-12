(ns megastrike.deployment-setup-test
  (:require [clojure.test :refer [deftest is]]
            [megastrike.deployment-setup :as setup]))

(def game
  {:forces [{:unit-group/keyword :blue :initiative 4 :unit-group/deployment :direction/n}
            {:unit-group/keyword :red :initiative 8 :unit-group/deployment :direction/s}]
   :units [{:unit/id "b1" :unit/battle-force :blue :unit/location {}}
           {:unit/id "b2" :unit/battle-force :blue :unit/location {}}
           {:unit/id "r1" :unit/battle-force :red :unit/location {}}]})

(deftest initiative-winner-placement-choice
  (is (= [:blue :red] (setup/force-order game)))
  (is (= [:red :blue] (setup/force-order (assoc game :deployment {:initiative-winner-places :first}))))
  (is (= [:blue :red] (setup/force-order (assoc game :deployment {:initiative-winner-places :second})))) )

(deftest alternating-placement-turns
  (is (= [:blue :red :blue] (setup/turn-order game)))
  (is (= [:red :blue :blue]
         (setup/turn-order (assoc game :deployment {:initiative-winner-places :first})))))

(deftest inward-facing-defaults
  (is (= :direction/s (setup/auto-facing game (first (:units game)))))
  (is (= :direction/n (setup/auto-facing game (last (:units game))))))
