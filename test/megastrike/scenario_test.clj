(ns megastrike.scenario-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :as t]
   [megastrike.scenario :as sut]))

(def scenario-folder (io/file "data/scenarios"))
(def test-scenario-path "data/scenarios/1stSomersetStrikers/1-ClashInTheCanyon.mms")
(def test-scenarios (filter #(.isFile %) (file-seq scenario-folder)))
(def test-data (sut/parse-scenario-file test-scenario-path))

(t/deftest parse-scenario-file
  (t/testing "Tests all scenario files"
    (run! #(t/is (some? (sut/parse-scenario-file %))) test-scenarios)
    (t/is (= 5 (count (:units test-data))))
    (t/is (some #{"Hunchback IIC"} (mapv :unit/full-name (:units test-data))))
    (t/is (= (:forces test-data)
             [{:unit-group/camo nil,
               :unit-group/deployment :direction/n,
               :unit-group/keyword :1stsomersetstrikers,
               :unit-group/name "1stSomersetStrikers",
               :unit-group/parent 1,
               :unit-group/subgroups [],
               :unit-group/player :player}
              {:unit-group/camo nil,
               :unit-group/deployment :direction/any,
               :unit-group/keyword :blackvision,
               :unit-group/name "BlackVision",
               :unit-group/parent 2,
               :unit-group/subgroups [],
               :unit-group/player :player}]))))

(t/deftest edn-save-file-writer
  (t/testing "Test conversion of a save file"
    (let [setup (sut/setup-scenario test-scenario-path)
          state (sut/edn-scenario-writer (:game setup))]
      (t/is (= (:forces state) (:forces test-data)))
      (t/is (= (:maps test-data) (:maps state)))
      (t/is (= (:lobby setup) (sut/set-maps state)))
      (t/is (= (mapv :unit/full-name (:units state))
               ["Axman AXM-2N"
                "Mauler MAL-1R"
                "Wolfhound WLF-2"
                "Vulture (Mad Dog) Prime"
                "Hunchback IIC"])))))
