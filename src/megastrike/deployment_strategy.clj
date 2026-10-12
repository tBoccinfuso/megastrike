(ns megastrike.deployment-strategy
  "Deterministic position preferences for already-legal deployment tiles.
  This namespace does not decide whether a hex is legal, or change phases."
  (:require [megastrike.deployment :as deployment]
            [megastrike.hexagons.hex :as hex]))

(defn offset-position [tile]
  (let [{:keys [x y]} (hex/hex->offset tile)]
    [(long x) (long y)]))

(defn distance [a b]
  (let [[ax ay] (offset-position a)
        [bx by] (offset-position b)]
    (+ (Math/abs (long (- ax bx))) (Math/abs (long (- ay by))))))

(defn preference [unit]
  (let [attacks (get-in unit [:unit/attacks :attack/regular] {})
        short (long (or (:attack/s attacks) 0))
        medium (long (or (:attack/m attacks) 0))
        long-range (long (or (:attack/l attacks) 0))
        movement (apply max 0 (filter number? (vals (or (:unit/move-modes unit) {}))))]
    (cond
      (and (pos? long-range) (>= long-range short)) :support
      (>= movement 10) :mobile
      (>= short medium) :frontline
      :else :balanced)))

(defn score-tile
  "Scores a tile from a list *already approved* by deployment/legal-tiles.
  Uses geometry and allied spacing only. No unsupported terrain bonuses.
  Returns a deterministic numeric score; higher is preferred."
  [game unit candidate]
  (let [tiles (vec (get-in game [:game-board :tiles]))
        ys (map (comp second offset-position) tiles)
        xs (map (comp first offset-position) tiles)
        [x y] (offset-position candidate)
        cx (/ (+ (apply min xs) (apply max xs)) 2.0)
        cy (/ (+ (apply min ys) (apply max ys)) 2.0)
        center-distance (+ (Math/abs (- x cx)) (Math/abs (- y cy)))
        deployed-allies (filter #(and (= (:unit/battle-force %) (:unit/battle-force unit))
                                      (deployment/deployed? %)
                                      (not= (:unit/id %) (:unit/id unit))) (:units game))
        nearest-ally (when (seq deployed-allies)
                       (apply min (map #(distance candidate (:unit/location %)) deployed-allies)))
        role (preference unit)
        center-weight (case role :frontline -2.0 :mobile 1.0 :support 1.5 -0.5)
        spread (if nearest-ally (min 6.0 (* 2.0 nearest-ally)) 0.0)
        clustering-penalty (if (and nearest-ally (< nearest-ally 2)) -8.0 0.0)]
    (+ (* center-weight center-distance) spread clustering-penalty)))

(defn rank-legal-tiles
  "Sort an externally validated candidate list; stable tie-break by offset coords.
  No placement is performed here."
  [game unit legal-candidates]
  (sort-by (fn [tile]
             (let [[x y] (offset-position tile)]
               [(- (score-tile game unit tile)) y x]))
           legal-candidates))

(defn best-legal-tile [game unit depth]
  (first (rank-legal-tiles game unit (deployment/legal-tiles game unit depth))))
