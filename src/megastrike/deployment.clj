(ns megastrike.deployment
  "Pure deployment-zone and occupancy checks shared by manual and AI placement.
  This module intentionally does not impose terrain or unit-type restrictions yet."
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

(defn legal-tiles
  "Return unoccupied board tiles inside the force's deployment zone.
  This is a geometry/occupancy filter, NOT a complete rules validator."
  [game unit depth]
  (let [force (some #(when (= (:unit-group/keyword %) (:unit/battle-force unit)) %) (:forces game))
        zone (:unit-group/deployment force)
        occupied (occupied-locations (:units game) (:unit/id unit))]
    (if (and force unit (pos-int? depth))
      (into [] (remove #(contains? occupied (location-key %)))
            (zone-tiles (:game-board game) zone depth))
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
