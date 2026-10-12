(ns megastrike.deployment-setup
  "Optional on-board placement order and initial facing. A scenario can set
  [:deployment :initiative-winner-places] to :first or :second. The default
  preserves MegaStrike's existing lower-initiative-first setup order."
  (:require [megastrike.deployment :as deployment]))

(defn force-order [game]
  (let [choice (get-in game [:deployment :initiative-winner-places] :second)
        winner-first? (= choice :first)
        sorted-forces (sort-by (juxt :initiative :unit-group/keyword) (:forces game))]
    (mapv :unit-group/keyword (if winner-first? (reverse sorted-forces) sorted-forces))))

(defn turn-order [game]
  (let [order (force-order game)
        remaining (frequencies (map :unit/battle-force
                                    (remove deployment/deployed? (:units game))))]
    (loop [counts remaining turns []]
      (let [round (filterv #(pos? (get counts % 0)) order)]
        (if (empty? round)
          turns
          (recur (reduce #(update %1 %2 dec) counts round)
                 (into turns round)))))))

(defn inward-facing [zone]
  (get {:n :direction/s
        :ne :direction/sw
        :e :direction/nw
        :se :direction/nw
        :s :direction/n
        :sw :direction/ne
        :w :direction/se
        :nw :direction/se}
       (deployment/normalize-zone zone)))

(defn auto-facing [game unit]
  (let [force (some #(when (= (:unit-group/keyword %) (:unit/battle-force unit)) %)
                    (:forces game))]
    (inward-facing (deployment/force-zone game force))))
