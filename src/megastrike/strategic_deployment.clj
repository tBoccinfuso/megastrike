(ns megastrike.strategic-deployment
  "Phase transitions for optional on-board automatic placement. This uses the
  current skirmish zone/occupancy validator, not complete Alpha Strike legality."
  (:require [megastrike.deployment :as deployment]
            [megastrike.deployment-setup :as setup]
            [megastrike.deployment-planner :as planner]
            [megastrike.phases :as phases]))

(defn initiative-order [game]
  (setup/force-order game))

(defn- face-placed-units [game placements]
  (let [placed-ids (set (map :unit placements))]
    (update game :units
            (fn [units]
              (mapv (fn [unit]
                      (if (contains? placed-ids (:unit/id unit))
                        (if-let [facing (setup/auto-facing game unit)]
                          (assoc unit :unit/facing facing)
                          unit)
                        unit))
                    units)))))

(defn auto-deploy-from-initiative [game depth]
  (if (not= :initiative (:current-phase game))
    {:game game :unplaced [] :placements [] :advanced? false}
    (let [deployment-game (phases/next-phase game)
          result (planner/plan deployment-game
                               {:force-order (initiative-order deployment-game)
                                :legal-tiles-fn #(deployment/legal-tiles %1 %2 depth)})
          placed-game (face-placed-units (:game result) (:placements result))
          done? (and (empty? (:unplaced result))
                     (every? deployment/deployed? (:units placed-game)))]
      (assoc result
             :game (if done? (phases/next-phase placed-game) placed-game)
             :advanced? done?))))

(defn auto-deploy-kevin-turns
  "During optional on-board manual deployment, play consecutive Kevin turns.
  The player retains every human turn. Failure leaves Kevin's turn pending.
  Uses exactly the same zone/occupancy provider as other skirmish placement."
  [game depth]
  (loop [current game placed []]
    (let [force-id (first (:turn-order current))
          force (some #(when (= force-id (:unit-group/keyword %)) %) (:forces current))]
      (if (or (not= :deployment (:current-phase current))
              (not= :kevin (:unit-group/player force)))
        {:game current :placements placed :blocked? false}
        (let [unit (first (filter #(and (= force-id (:unit/battle-force %))
                                        (not (:unit/acted? %))
                                        (not (deployment/deployed? %)))
                                  (:units current)))
              result (when unit
                       (planner/plan current {:force-order [force-id]
                                              :legal-tiles-fn (fn [g u]
                                                                (deployment/legal-tiles g u depth))}))
              placement (first (:placements result))]
          (if placement
            (recur (update (face-placed-units (:game result) [placement]) :turn-order rest) (conj placed placement))
            {:game current :placements placed :blocked? true}))))))
