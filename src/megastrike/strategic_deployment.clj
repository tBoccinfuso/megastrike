(ns megastrike.strategic-deployment
  "Phase transitions for optional on-board automatic placement. This uses the
  current skirmish zone/occupancy validator, not complete Alpha Strike legality."
  (:require [megastrike.deployment :as deployment]
            [megastrike.deployment-planner :as planner]
            [megastrike.phases :as phases]))

(defn initiative-order [game]
  ;; Ascending initiative is the existing game's deployment ordering convention.
  ;; The quick-start optional placement rule additionally gives the winner the
  ;; choice of first or second; the current skirmish mode has no such UI yet.
  (mapv :unit-group/keyword (sort-by (juxt :initiative :unit-group/keyword) (:forces game))))

(defn auto-deploy-from-initiative [game depth]
  (if (not= :initiative (:current-phase game))
    {:game game :unplaced [] :placements [] :advanced? false}
    (let [deployment-game (phases/next-phase game)
          result (planner/plan deployment-game
                               {:force-order (initiative-order deployment-game)
                                :legal-tiles-fn #(deployment/legal-tiles %1 %2 depth)})
          done? (and (empty? (:unplaced result))
                     (every? deployment/deployed? (:units (:game result))))]
      (assoc result
             :game (if done? (phases/next-phase (:game result)) (:game result))
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
            (recur (update (:game result) :turn-order rest) (conj placed placement))
            {:game current :placements placed :blocked? true}))))))
