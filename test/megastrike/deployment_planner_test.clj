(ns megastrike.deployment-planner-test
  (:require [clojure.test :refer [deftest is testing]]
            [megastrike.board :as board]
            [megastrike.deployment :as deployment]
            [megastrike.deployment-planner :as planner]))

(defn fixture []
  {:game-board (board/create-board 8 8)
   :forces [{:unit-group/keyword :blue :unit-group/deployment :direction/n}
            {:unit-group/keyword :red :unit-group/deployment :direction/s}]
   :units (mapv (fn [[side id]]
                  {:unit/id id :unit/battle-force side :unit/location {}
                   :unit/attacks {:attack/regular {:attack/s 2 :attack/m 1 :attack/l 0}}
                   :unit/move-modes {:move/walk 5}})
                [[:blue "b1"] [:blue "b2"] [:blue "b3"]
                 [:red "r1"] [:red "r2"]])})

(defn legal [game unit] (deployment/legal-tiles game unit 2))

(deftest alternating-forces
  (is (= ["r1" "b1" "r2" "b2" "b3"]
         (mapv :unit/id (planner/placement-order (fixture) [:red :blue])))))

(deftest planned-positions-are-legal-and-not-overlapping
  (let [initial (fixture)
        result (planner/plan initial {:force-order [:blue :red] :legal-tiles-fn legal})
        placements (:placements result)]
    (is (= 5 (count placements)))
    (is (empty? (:unplaced result)))
    (is (= 5 (count (set (map :location placements)))))
    (is (= ["b1" "r1" "b2" "r2" "b3"] (mapv :unit placements)))
    (is (every? (fn [{:keys [force unit location]}]
                  (let [original (some #(when (and (= force (:unit/battle-force %))
                                                   (= unit (:unit/id %))) %) (:units initial))]
                    (deployment/eligible-location? initial original location 2)))
                placements))
    (is (= initial (fixture)))))

(deftest repeatable-plan
  (let [game (fixture)
        opts {:force-order [:blue :red] :legal-tiles-fn legal}]
    (is (= (planner/plan game opts) (planner/plan game opts)))))

(deftest fails-closed-without-complete-validator
  (let [result (planner/plan (fixture) {:force-order [:blue :red]})]
    (is (empty? (:placements result)))
    (is (= 5 (count (:unplaced result))))
    (is (= (fixture) (:game result)))))

(deftest full-zone-does-not-reuse-a-hex
  (let [initial (fixture)
        one-tile (first (legal initial (first (:units initial))))
        result (planner/plan initial
                             {:force-order [:blue :red]
                              :legal-tiles-fn (fn [game unit]
                                                (filter #(= (deployment/location-key %) (deployment/location-key one-tile))
                                                        (legal game unit)))})]
    (is (= 1 (count (:placements result))))
    (is (= 4 (count (:unplaced result))))))

(deftest already-deployed-is-preserved
  (let [base (fixture)
        tile (first (legal base (first (:units base))))
        game (assoc-in base [:units 0 :unit/location] (deployment/location-key tile))
        result (planner/plan game {:force-order [:blue :red] :legal-tiles-fn legal})]
    (is (= (deployment/location-key tile) (get-in result [:game :units 0 :unit/location])))
    (is (= 4 (count (:placements result))))))

(deftest unknown-force-remains-unplaced
  (let [result (planner/plan (fixture) {:force-order [:blue] :legal-tiles-fn legal})]
    (is (= 3 (count (:placements result))))
    (is (= #{[:red "r1"] [:red "r2"]} (set (:unplaced result))))))
