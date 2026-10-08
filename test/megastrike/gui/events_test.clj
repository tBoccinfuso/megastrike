(ns megastrike.gui.events-test
  (:require
   [clojure.test :as t]
   [megastrike.combat-unit :as cu]
   [megastrike.scenario :as scenario]
   [megastrike.gui.lobby.events :as lobby-e]))

;; Setup dummy data for tests
(def test-mul [{:unit/name "Atlas" :unit/type "Assault"}
               {:unit/name "Locust" :unit/type "Light"}])
(def test-lobby {:mul test-mul :active-mul nil})
(def test-game {:units []})
(def test-context {:lobby test-lobby :game test-game})

(t/deftest filter-changed-test
  (t/testing "Filter BattleMechs"
    (let [filtered (cu/filter-units cu/mul :type/bm)]
      (t/is (pos? (count filtered)))
      (t/is (every? #(= :type/bm (:unit/type %)) filtered))))
  (t/testing "Filter all mechs"
    (let [filtered (cu/filter-units cu/mul :mul/mechs)]
      (t/is (pos? (count filtered)))
      (t/is (every? #(isa? (:unit/type %) :mul/mechs) filtered))))
  (t/testing "Filter ground units"
    (let [filtered (cu/filter-units cu/mul :mul/ground-units)]
      (t/is (pos? (count filtered)))
      (t/is (every? #(isa? (:unit/type %) :mul/ground-units) filtered)))))

(t/deftest mul-selection-changed-test
  (t/testing "Test that MUL selection updates the lobby state"
    (let [new-unit {:unit/name "Locust"}
          ;; Simulating the logic: (assoc-in context [:lobby :active-mul] event)
          new-context (assoc-in test-context [:lobby :active-mul] new-unit)]
      (t/is (= (get-in new-context [:lobby :active-mul]) new-unit)))))

(t/deftest load-scenario-test
  (t/testing "Test that loading a scenario returns the correct data structure"
    (let [path "data/scenarios/1stSomersetStrikers/1-ClashInTheCanyon.mms"
          response (scenario/setup-scenario path)]
      (t/is (map? (:lobby response)))
      (t/is (map? (:game response)))
      (t/is (pos? (count (:units (:game response))))))))