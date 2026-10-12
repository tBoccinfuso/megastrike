(ns megastrike.gui.movement-selection-test
  (:require [clojure.test :refer [deftest is testing]]
            [megastrike.gui.events :as events]))

(deftest movement-selection-validation
  (let [game {:current-phase :movement :turn-order [:blue]}
        blue {:unit/id "b1" :unit/battle-force :blue :unit/acted? false}
        red {:unit/id "r1" :unit/battle-force :red :unit/acted? false}]
    (testing "current unacted unit is selectable"
      (is (nil? (events/movement-selection-error game blue))))
    (testing "opposing force is not selectable for movement"
      (is (= "It's not this unit's movement turn."
             (events/movement-selection-error game red))))
    (testing "acted units are not selectable"
      (is (= "That unit has already moved."
             (events/movement-selection-error game (assoc blue :unit/acted? true)))))
    (testing "outside movement phase this validator does not restrict actions"
      (is (nil? (events/movement-selection-error (assoc game :current-phase :combat) red))))))
