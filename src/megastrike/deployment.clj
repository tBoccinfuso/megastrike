(ns megastrike.deployment
  "Shared deployment validation for manual and automated placement.
  Advanced movement and terrain rules require verified scenario/unit metadata."
  (:require [megastrike.hexagons.hex :as hex]))

(defn location-key [tile]
  (select-keys tile [:hex/p :hex/q :hex/r]))

(defn deployed? [unit]
  (let [loc (:unit/location unit)]
    (and (map? loc) (every? number? ((juxt :hex/p :hex/q :hex/r) loc)))))

(defn normalize-zone [zone]
  (when (keyword? zone)
    (keyword (name zone))))

(defn zone-tiles
  "Tiles within depth rows/columns of a named board edge. Diagonal zones
  are intersections of the corresponding cardinal edges."
  [board zone depth]
  (let [tiles (vec (:tiles board))
        zone (normalize-zone zone)
        depth (max 1 (long depth))]
    (if (or (empty? tiles) (not (contains? #{:n :ne :e :se :s :sw :w :nw} zone)))
      []
      (let [ps (map :hex/p tiles)
            ;; Cube coordinates cannot be used to establish board rows on their own.
            ;; Derive offset coordinates using the project's existing hex conversion.
            coords (mapv (fn [tile] [tile ((juxt :x :y) (hex/hex->offset tile))]) tiles)
            xs (map (comp first second) coords)
            ys (map (comp second second) coords)
            min-x (apply min xs)
            max-x (apply max xs)
            min-y (apply min ys)
            max-y (apply max ys)
            inside? (fn [[_ [x y]]]
                      (and (if (#{:n :ne :nw} zone) (< (- y min-y) depth) true)
                           (if (#{:s :se :sw} zone) (< (- max-y y) depth) true)
                           (if (#{:w :nw :sw} zone) (< (- x min-x) depth) true)
                           (if (#{:e :ne :se} zone) (< (- max-x x) depth) true)))]
        (mapv first (filter inside? coords))))))

(defn occupied-locations [units exclude-id]
  (into #{} (comp (remove #(= (:unit/id %) exclude-id))
                  (filter deployed?)
                  (map (comp location-key :unit/location))) units))

(defn deployment-depth
  "Optional scenario-defined number of hex rows. Falls back to the existing
  three-row skirmish configuration until an explicit map scale is available."
  [game default-depth]
  (let [n (or (get-in game [:deployment :depth-hexes])
              (:deployment/depth-hexes game)
              default-depth)]
    (when (and (integer? n) (pos? n)) n)))

(defn deployment-reason
  "Return short user-facing explanation, or nil for a permitted tile.
  Explicit :deployment/blocked? tile and :deployment/forbidden-terrain values
  are scenario/map metadata. No speculative terrain-to-unit rules are inferred."
  [game unit tile default-depth]
  (let [depth (deployment-depth game default-depth)
        force (some #(when (= (:unit-group/keyword %) (:unit/battle-force unit)) %) (:forces game))
        actual (first (filter #(= (location-key %) (location-key tile))
                              (get-in game [:game-board :tiles])))
        terrain (:terrain actual)
        forbidden (set (or (get-in game [:deployment :forbidden-terrain])
                           (:deployment/forbidden-terrain game)))
        zone (when depth (zone-tiles (:game-board game) (:unit-group/deployment force) depth))]
    (cond
      (nil? unit) "Select a unit first."
      (:unit/acted? unit) "This unit is already deployed."
      (nil? actual) "Choose a hex on the map."
      (or (nil? force) (nil? depth) (empty? zone)) "No valid deployment zone is configured."
      (not (some #(= (location-key %) (location-key actual)) zone)) "Outside your deployment zone."
      (contains? (occupied-locations (:units game) (:unit/id unit)) (location-key actual))
      "That hex is occupied."
      (:deployment/blocked? actual) "Deployment is blocked on that hex."
      (contains? forbidden terrain) "Deployment is restricted on that terrain."
      :else nil)))

(defn legal-tiles
  "Return only tiles allowed by the shared deployment validation."
  [game unit default-depth]
  (let [depth (deployment-depth game default-depth)
        force (some #(when (= (:unit-group/keyword %) (:unit/battle-force unit)) %) (:forces game))]
    (if (and unit force depth)
      (into [] (remove #(deployment-reason game unit % default-depth))
            (zone-tiles (:game-board game) (:unit-group/deployment force) depth))
      [])))

(defn eligible-location? [game unit tile depth]
  (boolean (some #(= (location-key %) (location-key tile)) (legal-tiles game unit depth))))

(defn auto-deploy
  "Place every undeployed unit using available zone tiles. Returns {:game ... :unplaced [...]}.
  Locations are checked against the shared zone/occupancy validator after each placement.
  No special terrain or scenario rules are imposed beyond those in legal-tiles."
  [game depth]
  (reduce (fn [{:keys [game unplaced]} unit]
            (if (deployed? unit)
              {:game game :unplaced unplaced}
              (if-let [tile (first (legal-tiles game unit depth))]
                {:game (update game :units
                               (fn [units]
                                 (mapv (fn [candidate]
                                         (if (= (:unit/id candidate) (:unit/id unit))
                                           (assoc candidate :unit/location (location-key tile) :unit/acted? true)
                                           candidate)) units)))
                 :unplaced unplaced}
                {:game game :unplaced (conj unplaced (:unit/id unit))})))
          {:game game :unplaced []}
          (:units game)))
