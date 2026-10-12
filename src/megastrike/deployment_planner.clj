(ns megastrike.deployment-planner
  "Pure force-wide placement planning. The caller supplies a validated legal-tile
  provider and the force placement order. This namespace does not advance phases
  or decide scenario-specific legalities."
  (:require [megastrike.deployment :as deployment]
            [megastrike.deployment-strategy :as strategy]))

(defn placement-order
  "Round-robin units from forces in the exact order supplied by the caller.
  Existing deployed units are excluded. Unknown forces and duplicates are ignored.
  For Alpha Strike optional on-board preplacement, the caller must resolve the
  initiative winner's first/second choice before passing force-order."
  [game force-order]
  (let [groups (mapv (fn [force-id]
                       (vec (filter #(and (= force-id (:unit/battle-force %))
                                          (not (deployment/deployed? %)))
                                    (:units game))))
                     (distinct force-order))]
    (loop [remaining groups result []]
      (if (every? empty? remaining)
        result
        (recur (mapv #(if (seq %) (subvec % 1) %) remaining)
               (into result (keep first remaining)))))))

(defn- with-location [game unit tile]
  (update game :units
          (fn [units]
            (mapv (fn [candidate]
                    (if (and (= (:unit/id candidate) (:unit/id unit))
                             (= (:unit/battle-force candidate) (:unit/battle-force unit)))
                      (assoc candidate :unit/location (deployment/location-key tile)
                                     :unit/acted? true)
                      candidate))
                  units))))

(defn plan
  "Build a deterministic proposed deployment, without changing the input game.

  options:
    :force-order - already-resolved alternating placement order
    :legal-tiles-fn - (fn [current-game unit] => *fully validated* legal tiles)

  Returns {:game ... :placements [...] :unplaced [...]}. A caller must supply
  the legal validator; geometry-only deployment/legal-tiles is NOT sufficient
  to claim full Alpha Strike terrain / scenario legality. If the provider is
  absent, the planner leaves every undeployed unit unplaced. Units belonging to
  an unlisted force are also left unplaced, never silently skipped.
  "
  [game {:keys [force-order legal-tiles-fn]}]
  (let [order (placement-order game force-order)
        ordered-ids (set (map (juxt :unit/battle-force :unit/id) order))
        unlisted (filterv #(and (not (deployment/deployed? %))
                                (not (contains? ordered-ids [(:unit/battle-force %) (:unit/id %)])))
                          (:units game))]
    (reduce (fn [{:keys [game placements unplaced]} unit]
              (let [candidates (if legal-tiles-fn
                                 (or (legal-tiles-fn game unit) [])
                                 [])
                    tile (when (seq candidates)
                           (first (strategy/rank-legal-tiles game unit candidates)))]
                (if tile
                  {:game (with-location game unit tile)
                   :placements (conj placements {:force (:unit/battle-force unit)
                                                 :unit (:unit/id unit)
                                                 :location (deployment/location-key tile)})
                   :unplaced unplaced}
                  {:game game :placements placements
                   :unplaced (conj unplaced [(:unit/battle-force unit) (:unit/id unit)])})))
            {:game game :placements []
             :unplaced (mapv (juxt :unit/battle-force :unit/id) unlisted)}
            order)))
