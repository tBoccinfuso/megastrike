(ns megastrike.skirmish-pv)

;; These are MegaStrike skirmish presets, not compulsory Alpha Strike force sizes.
(def brackets
  {:intro {:label "Introductory (150–200 PV)" :minimum 150 :maximum 200 :target 180}
   :standard {:label "Standard (250–350 PV)" :minimum 250 :maximum 350 :target 300}
   :large {:label "Large (400+ PV)" :minimum 400 :maximum nil :target 500}})

(defn within-bracket? [bracket total]
  (if-let [{:keys [minimum maximum]} (get brackets bracket)]
    (and (<= minimum total) (or (nil? maximum) (<= total maximum)))
    true))

(defn validation-message [bracket your-pv enemy-pv]
  (when-let [{:keys [label]} (get brackets bracket)]
    (cond
      (not (within-bracket? bracket your-pv))
      (str "Your force has " your-pv " PV. " label " requires "
           (if (= bracket :large) "at least 400 PV." (str (:minimum (brackets bracket)) " to " (:maximum (brackets bracket)) " PV.")))
      (not (within-bracket? bracket enemy-pv))
      (str "Enemy force has " enemy-pv " PV. " label " requires "
           (if (= bracket :large) "at least 400 PV." (str (:minimum (brackets bracket)) " to " (:maximum (brackets bracket)) " PV."))))))

(defn valid-pool [catalog mechs-only?]
  (->> catalog
       (filter #(and (integer? (:unit/base-pv %)) (pos? (:unit/base-pv %))))
       (filter #(or (not mechs-only?) (= :type/bm (:unit/type %))))
       vec))

(defn random-roster
  "Randomized, bounded greedy search for a roster inside the selected PV range.
   Returns MUL records, or nil when no roster was found. Does not create combat units."
  [catalog bracket mechs-only?]
  (when-let [{:keys [minimum maximum target]} (get brackets bracket)]
    (let [pool (valid-pool catalog mechs-only?)
          cap (or maximum (+ target 60))]
      (when (seq pool)
        (some identity
              (repeatedly 120
                (fn []
                  (loop [picked [] total 0 remaining 45]
                    (cond
                      (and (>= total minimum) (<= total cap)) picked
                      (zero? remaining) nil
                      :else (let [candidates (filter #(<= (+ total (:unit/base-pv %)) cap) pool)]
                              (if-let [unit (when (seq candidates) (rand-nth (vec candidates)))]
                                (recur (conj picked unit) (+ total (:unit/base-pv unit)) (dec remaining))
                                nil)))))))))))
