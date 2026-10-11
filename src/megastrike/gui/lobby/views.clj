(ns megastrike.gui.lobby.views
  (:require
   [cljfx.api :as fx]
   [cljfx.ext.table-view :as tables]
   [clojure.string :as str]
   [com.brunobonacci.mulog :as mu]
   [megastrike.battle-force :as battle-force]
   [megastrike.combat-unit :as cu]
   [megastrike.gui.elements :as elements]
   [megastrike.gui.events :as events]
   [megastrike.gui.subs :as subs]
   [megastrike.movement :as movement]
   [megastrike.pilot :as pilot]))

(defn filter-button
  [{:keys [fx/context values text]}]
  {:fx/type :button
   :cursor :hand
   :text text
   :style-class (if (= values (fx/sub-val context get-in [:lobby :mul-category] :mul/ground-units))
                  ["unit-filter-button" "selected-filter"]
                  ["unit-filter-button"])
   :on-action {:event-type ::events/filter-changed
               :fx/sync true
               :values values}})

(defn mul-filter-buttons [{:keys [fx/context]}]
  {:fx/type :v-box
   :spacing 6
   :children [{:fx/type :label :text "FILTER BY UNIT CATEGORY" :style-class ["unit-filter-caption"]}
              {:fx/type :h-box
   :spacing 7
   :alignment :center-left
   :children [{:fx/type filter-button :values :mul/ground-units :text "GROUND UNITS"}
              {:fx/type filter-button :values :type/bm :text "BATTLEMECHS"}
              {:fx/type filter-button :values :mul/mechs :text "ALL MECHS"}
              {:fx/type filter-button :values :mul/vehicle :text "VEHICLES"}
              {:fx/type filter-button :values :mul/infantry :text "INFANTRY"}
              {:fx/type filter-button :values :mul/conventional :text "CONVENTIONAL"}]}]})

(defn unit-type-label [unit]
  (case (:unit/type unit)
    :type/bm "BattleMech"
    :type/pm "ProtoMech"
    :type/im "IndustrialMech"
    :type/cv "Combat Vehicle"
    :type/sv "Support Vehicle"
    :type/ci "Infantry"
    :type/ba "Battle Armor"
    :type/as "Aerospace"
    :type/af "Aerospace"
    (-> (:unit/type unit) name str/upper-case)))

(defn abilities-label [unit]
  (let [abilities (->> (:unit/abilities unit)
                       vals
                       (keep :ability/output)
                       (remove str/blank?)
                       sort)]
    (if (seq abilities) (str/join ", " abilities) "None")))

(defn unit-column [title width render]
  {:fx/type :table-column
   :text title
   :pref-width width
   :sortable true
   :comparator (fn [a b]
                 (let [av (render a) bv (render b)]
                   (cond
                     (and (number? av) (number? bv)) (compare av bv)
                     :else (compare (str (or av "")) (str (or bv ""))))))
   :cell-value-factory identity
   :cell-factory {:fx/cell-type :table-cell
                  :describe (fn [unit] {:text (str (or (render unit) "–"))})}})

(defn mul-table [{:keys [fx/context]}]
  (let [mul (subs/mul context)
        selected (subs/active-mul context)]
    {:fx/type tables/with-selection-props
     :props {:selection-mode :single
             :on-selected-item-changed {:event-type ::events/mul-selection-changed :fx/sync true}
             :selected-item selected}
     :desc {:fx/type :table-view
            :style-class ["unit-browser-table"]
            :pref-height 350
            :column-resize-policy :constrained
            :placeholder {:fx/type :label :text "No matching units. Try another search or category."}
            :columns [(unit-column "UNIT" 245 :unit/full-name)
                      (unit-column "TYPE" 105 unit-type-label)
                      (unit-column "PV" 48 :unit/base-pv)
                      (unit-column "MOVE" 72 movement/print-movement)
                      (unit-column "TMM" 45 movement/base-tmm)
                      (unit-column "ARM" 48 #(get-in % [:unit/armor :toughness/current]))
                      (unit-column "STR" 48 #(get-in % [:unit/structure :toughness/current]))]
            :items mul}}))

(def mul-chassis-search
  {:fx/type :h-box
   :spacing 9
   :alignment :center-left
   :children [{:fx/type elements/text-input
               :label "Search units"
               :ks [:lobby :mul-search-term]}
              {:fx/type :button
               :text "SEARCH"
               :cursor :hand
               :on-action {:event-type ::events/filter-mul :fx/sync true :field :unit/full-name}}]})

(defn unit-detail-row [label value]
  {:fx/type :h-box :spacing 12 :alignment :center-left
   :children [{:fx/type :label :text label :min-width 95 :style-class ["muted-label"]}
              {:fx/type :label :text (str (or value "–")) :wrap-text true}]})

(defn unit-details [{:keys [fx/context]}]
  (let [unit (subs/active-mul context)]
    {:fx/type :v-box :spacing 11 :style-class ["unit-browser-details"]
     :pref-width 255 :min-width 230
     :children (if unit
                 [{:fx/type :label :text (:unit/full-name unit) :wrap-text true :style-class ["skirmish-heading"]}
                  (unit-detail-row "Type" (unit-type-label unit))
                  (unit-detail-row "Base PV" (:unit/base-pv unit))
                  (unit-detail-row "Movement" (movement/print-movement unit))
                  (unit-detail-row "TMM" (:unit/tmm unit))
                  (unit-detail-row "Armor" (get-in unit [:unit/armor :toughness/current]))
                  (unit-detail-row "Structure" (get-in unit [:unit/structure :toughness/current]))
                  {:fx/type :separator}
                  {:fx/type :label :text "SPECIAL ABILITIES" :style-class ["muted-label"]}
                  {:fx/type :label :text (abilities-label unit) :wrap-text true}]
                 [{:fx/type :label :text "UNIT DETAILS" :style-class ["skirmish-heading"]}
                  {:fx/type :label :text "Select a unit in the table to inspect its statistics." :wrap-text true :style-class ["muted-label"]}])}))

(defn new-unit-buttons [{:keys [fx/context]}]
  {:fx/type :grid-pane
   :hgap 12
   :vgap 8
   :alignment :center-left
   :style-class ["unit-browser-footer"]
   :children [{:fx/type :label :text "Pilot Name" :grid-pane/row 0 :grid-pane/column 0}
              {:fx/type :text-field
               :pref-width 180
               :text (str (fx/sub-val context get-in [:lobby :pilot-name]))
               :on-text-changed {:event-type ::events/text-input :fx/sync true :ks [:lobby :pilot-name]}
               :grid-pane/row 0 :grid-pane/column 1}
              {:fx/type :label :text "Pilot Skill" :grid-pane/row 0 :grid-pane/column 2}
              {:fx/type :text-field
               :pref-width 90
               :text (str (fx/sub-val context get-in [:lobby :pilot-skill]))
               :on-text-changed {:event-type ::events/text-input :fx/sync true :ks [:lobby :pilot-skill]}
               :grid-pane/row 0 :grid-pane/column 3}]})

(def mul-pane
  {:fx/type :v-box
   :spacing 12
   :fill-width true
   :style-class ["unit-browser"]
   :children [{:fx/type :label :text "ADD UNIT TO FORCE" :style-class ["skirmish-heading"]}
              {:fx/type mul-filter-buttons}
              mul-chassis-search
              {:fx/type :h-box :spacing 14
               :children [{:fx/type :v-box :h-box/hgrow :always :spacing 6
                           :children [{:fx/type mul-table}]}
                          {:fx/type unit-details}]}
              {:fx/type new-unit-buttons}]})

(defn mul-dialog
  [_]
  {:fx/type elements/confirmation-pane
   :dialog-id :mul-dialog
   :on-confirmed {:event-type ::events/add-unit}
   :button {:text "Add new unit to selected force"}
   :dialog-pane {:content mul-pane}})

(defn forces-table
  [{:keys [fx/context]}]
  (let [forces (subs/forces context)
        selected (subs/lobby-active-force context)
        units (subs/units context)]
    (if (empty? forces)
      {:fx/type :label
       :text "Add a force."}
      {:fx/type tables/with-selection-props
       :props {:selection-mode :single
               :on-selected-item-changed {:event-type ::events/force-selection-changed}
               :selected-item selected}
       :desc {:fx/type :table-view
              :columns [{:fx/type :table-column
                         :text "Name"
                         :cell-value-factory identity
                         :cell-factory {:fx/cell-type :table-cell
                                        :describe (fn [x] {:text (:unit-group/name x)})}}
                        {:fx/type :table-column
                         :text "Player"
                         :cell-value-factory identity
                         :cell-factory {:fx/cell-type :table-cell
                                        :describe (fn [x] {:text (name (or (:unit-group/player x) :none))})}}
                        {:fx/type :table-column
                         :text "Deployment"
                         :cell-value-factory identity
                         :cell-factory {:fx/cell-type :table-cell
                                        :describe (fn [x] {:text (or (name (:unit-group/deployment x)) (name :none))})}}
                        {:fx/type :table-column
                         :text "Unit Count"
                         :cell-value-factory identity
                         :cell-factory {:fx/cell-type :table-cell
                                        :describe (fn [x] {:text (prn-str (or (count (battle-force/force-units x units)) 0))})}}
                        {:fx/type :table-column
                         :text "Total PV"
                         :cell-value-factory identity
                         :cell-factory {:fx/cell-type :table-cell
                                        :describe (fn [x] {:text (prn-str (or (battle-force/force-pv x units) 0))})}}]

              :items forces}})))

(defn force-creation-dialog
  [{:keys [fx/context]}]
  {:fx/type elements/confirmation-pane
   :dialog-id :force-creation-dialog
   :on-confirmed {:event-type ::events/add-force}
   :button {:text "Add/Edit force"}
   :dialog-pane {:content
                 {:fx/type :v-box
                  :spacing 5
                  :fill-width true
                  :alignment :top-center
                  :children
                  [{:fx/type :label :text "Add/Edit Force"}
                   {:fx/type elements/text-input
                    :label "Force Name"
                    :ks [:gui :force-creation-dialog :name]}
                   {:fx/type elements/text-input
                    :label "Force Deployment"
                    :ks [:gui :force-creation-dialog :zone]}
                   {:fx/type :h-box
                    :spacing 5
                    :children
                    [{:fx/type :text :text "Human or AI?"}
                     {:fx/type :choice-box
                      :items [:local-player :kevin :remote-player]
                      :value :local-player
                      :on-value-changed {:event-type ::events/change-player}}]}
                   (if (subs/force-camo context)
                     {:fx/type :button
                      :background {:images (list (subs/force-camo context))}
                      :text "Change Camo"
                      :on-action {:event-type ::events/select-camo :fx/sync true}}
                     {:fx/type :button
                      :text "Select Camo"
                      :on-action {:event-type ::events/select-camo :fx/sync true}})]}}})

(def force-pane
  {:fx/type :v-box
   :spacing 5
   :fill-width true
   :grid-pane/row 0
   :grid-pane/column 0
   :grid-pane/hgrow :always
   :grid-pane/vgrow :always
   :alignment :top-center
   :children [{:fx/type force-creation-dialog}
              {:fx/type forces-table}
              {:fx/type mul-dialog}]})

(defn units-table [{:keys [fx/context]}]
  (let [units (subs/units context)
        forces (subs/forces context)
        selected nil]
    (if (empty? units)
      {:fx/type :label
       :text "Please add a unit."}
      {:fx/type tables/with-selection-props
       :props {:selection-mode :single
               :on-selected-item-changed {:event-type ::events/unit-selection-changed :fx/sync true}
               :selected-item selected}
       :desc {:fx/type :table-view
              :columns [{:fx/type :table-column
                         :text "Unit"
                         :cell-value-factory identity
                         :cell-factory {:fx/cell-type :table-cell
                                        :describe (fn [x] {:text (:unit/id x)})}}
                        {:fx/type :table-column
                         :text "Image"
                         :cell-value-factory identity
                         :cell-factory {:fx/cell-type :table-cell
                                        :describe (fn [x] {:graphic {:fx/type elements/draw-sprite
                                                                     :unit x
                                                                     :bf (battle-force/select-force forces (:unit/battle-force x))
                                                                     :x 0
                                                                     :y 0
                                                                     :shift 0}})}}
                        {:fx/type :table-column
                         :text "Pilot"
                         :cell-value-factory identity
                         :cell-factory {:fx/cell-type :table-cell
                                        :describe (fn [x] {:text (pilot/display (:unit/pilot x))})}}
                        {:fx/type :table-column
                         :text "PV"
                         :cell-value-factory identity
                         :cell-factory {:fx/cell-type :table-cell
                                        :describe (fn [x] {:text (prn-str (cu/pv x))})}}]
              :items units}})))

(def unit-pane
  {:fx/type :v-box
   :spacing 5
   :fill-width true
   :alignment :top-center
   :grid-pane/row 0
   :grid-pane/column 1
   :grid-pane/row-span 2
   :grid-pane/hgrow :always
   :grid-pane/vgrow :always
   :children [{:fx/type :label
               :text "Unit List"}
              {:fx/type units-table}]})

(defn map-grid
  [{:keys [fx/context]}]
  (let [width (subs/map-width context)
        height (subs/map-height context)
        boards (or (subs/map-boards context) [])]
    {:fx/type :grid-pane
     :children (for [x (range width)
                     y (range height)]
                 {:fx/type :button
                  :grid-pane/column x
                  :grid-pane/row y
                  :text (:name (nth boards (+ (* y width) x) {:name "None"}))
                  :id (str (+ (* y width) x))
                  :on-action {:event-type ::events/load-mapboard :id (+ (* y width) x)}})}))

(def map-pane
  {:fx/type :v-box
   :spacing 5
   :fill-width true
   :alignment :top-center
   :grid-pane/row 1
   :grid-pane/column 0
   :grid-pane/hgrow :always
   :grid-pane/vgrow :always
   :children [{:fx/type :label
               :text "Map Setup"}
              {:fx/type elements/text-input
               :label "Map Width (in boards)"
               :ks [:game :map-width]}
              {:fx/type elements/text-input
               :label "Map Height (in boards)"
               :ks [:game :map-height]}
              {:fx/type map-grid}
              ;; {:fx/type :button
              ;;  :text "Load Test Game"
              ;;  :on-action {:event-type ::events/load-save :fx/sync true}}
              {:fx/type :button
               :text "Load Scenario"
               :on-action {:event-type ::events/load-scenario :fx/sync true}}
              {:fx/type :button
               :text "Message Server"
               :on-action {:event-type ::events/message-server :fx/sync true}}
              {:fx/type :button
               :text "Kill Server"
               :on-action {:event-type ::events/close-server :fx/sync true}}
              {:fx/type :button
               :text "Launch Game"
               :on-action {:event-type ::events/launch-game :fx/sync true :view :game}}]})
