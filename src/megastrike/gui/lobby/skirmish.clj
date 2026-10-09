(ns megastrike.gui.lobby.skirmish
  (:require
   [cljfx.api :as fx]
   [cljfx.ext.table-view :as tables]
   [clojure.string :as str]
   [megastrike.battle-force :as bf]
   [megastrike.combat-unit :as cu]
   [megastrike.gui.elements :as elements]
   [megastrike.gui.events :as events]
   [megastrike.gui.lobby.views :as lobby]
   [megastrike.gui.subs :as subs]))

(def deployments ["N" "NE" "E" "SE" "S" "SW" "W" "NW" "EDG" "CTR"])
(def controllers ["Human" "Kevin (AI)"])

(defn controller-label [controller]
  (case controller
    :kevin "Kevin (AI)"
    :local-player "Human"
    "Human"))

(defn field [field-name force-id label value]
  {:fx/type :h-box
   :alignment :center-left
   :spacing 12
   :children [{:fx/type :label :text label :min-width 82 :style-class ["muted-label"]}
              (case field-name
                :unit-group/name
                {:fx/type :text-field :text (or value "") :h-box/hgrow :always
                 :on-text-changed {:event-type ::events/skirmish-change-force :force-id force-id :field field-name}}
                :unit-group/player
                {:fx/type :combo-box :items controllers :value (controller-label value)
                 :max-width Double/MAX_VALUE :h-box/hgrow :always
                 :on-value-changed {:event-type ::events/skirmish-change-force :force-id force-id :field field-name}}
                :unit-group/deployment
                {:fx/type :combo-box :items deployments
                 :value (str/upper-case (clojure.core/name (or value :direction/n)))
                 :max-width Double/MAX_VALUE :h-box/hgrow :always
                 :on-value-changed {:event-type ::events/skirmish-change-force :force-id force-id :field field-name}})]})

(defn unit-column [heading render]
  {:fx/type :table-column
   :text heading
   :cell-value-factory identity
   :cell-factory {:fx/cell-type :table-cell
                  :describe (fn [unit] {:text (str (render unit))})}})

(defn roster [{:keys [fx/context force]}]
  (let [units (vec (filter #(= (:unit/battle-force %) (:unit-group/keyword force)) (subs/units context)))]
    {:fx/type tables/with-selection-props
     :props {:selection-mode :single
             :on-selected-item-changed {:event-type ::events/skirmish-select-unit :force-index (if (= (:unit-group/parent force) 1) 0 1)}}
     :desc {:fx/type :table-view
            :style-class ["roster-table"]
            :min-height 190
            :pref-height 260
            :columns [(unit-column "UNIT" :unit/full-name)
                      (unit-column "PV" cu/pv)
                      (unit-column "SKILL" #(get-in % [:unit/pilot :pilot/skill]))
                      (unit-column "MOVE" #(or (first (vals (:unit/move-modes %))) "–"))]
            :items units
            :placeholder {:fx/type :label
                          :text "No units yet — click ADD UNIT to build this force."
                          :style-class ["roster-empty-message"]}}}))

(defn force-panel [{:keys [fx/context force index]}]
  (let [force-id (:unit-group/keyword force)
        all-units (subs/units context)
        units (filter #(= (:unit/battle-force %) force-id) all-units)
        pv (reduce + (map cu/pv units))
        dialog-id (if (zero? index) :skirmish-add-force-1 :skirmish-add-force-2)
        selected (get-in (subs/lobby context) [:skirmish-selected index])]
    {:fx/type :v-box
     :spacing 12
     :style-class ["skirmish-force"]
     :pref-width 470
     :children [{:fx/type :label :text (if (zero? index) "YOUR FORCE" "ENEMY FORCE")
                 :style-class ["skirmish-heading"]}
                (field :unit-group/name force-id "Name" (:unit-group/name force))
                (field :unit-group/player force-id "Player" (:unit-group/player force))
                (field :unit-group/deployment force-id "Deployment" (:unit-group/deployment force))
                {:fx/type roster :force force}
                {:fx/type :h-box :alignment :center-left :spacing 8
                 :children [{:fx/type elements/confirmation-pane
                             :dialog-id dialog-id
                             :on-confirmed {:event-type ::events/skirmish-add-unit :force-id force-id}
                             :button {:cursor :hand :text "ADD UNIT"
                                      :style-class ["skirmish-primary" "skirmish-action-button"]
                                      :style "-fx-padding: 10 20 10 20; -fx-min-width: 118px; -fx-min-height: 36px;"}
                             :dialog-pane {:pref-width 880 :content lobby/mul-pane}}
                            {:fx/type :button :cursor :hand :text "REMOVE"
                             :disable (nil? selected)
                             :on-action {:event-type ::events/skirmish-remove-unit :unit-id selected}}
                            {:fx/type :region :h-box/hgrow :always}
                            {:fx/type :label :text (str "TOTAL PV  " pv) :style-class ["skirmish-pv"]}]}]}))

(defn board-size-field [{:keys [fx/context label ks]}]
  {:fx/type :h-box :alignment :center-left :spacing 10
   :children [{:fx/type :label :text label :min-width 115 :style-class ["muted-label"]}
              {:fx/type :text-field :text (str (fx/sub-val context get-in ks))
               :pref-width 95
               :on-text-changed {:event-type ::events/text-input :fx/sync true :ks ks}}]})

(defn board-selector [{:keys [fx/context]}]
  (let [width (subs/map-width context)
        height (subs/map-height context)
        boards (or (subs/map-boards context) [])]
    {:fx/type :grid-pane :hgap 8 :vgap 8
     :children (vec (for [y (range height)
                          x (range width)
                          :let [idx (+ (* y width) x)
                                board (nth boards idx nil)
                                label (or (:name board) "SELECT MAP")]]
                      {:fx/type :button :cursor :hand :grid-pane/column x :grid-pane/row y
                       :text label :style-class ["map-select-button" "skirmish-action-button"]
                       :style "-fx-padding: 10 20 10 20; -fx-min-width: 145px; -fx-min-height: 36px; -fx-text-fill: #d5d7d8; -fx-background-color: #202426; -fx-border-color: #c68a3c;"
                       :on-action {:event-type ::events/load-mapboard :id idx}}))}))

(defn battlefield-panel [{:keys [fx/context]}]
  {:fx/type :v-box :spacing 14 :style-class ["skirmish-battlefield"]
   :children [{:fx/type :label :text "BATTLEFIELD SETUP" :style-class ["skirmish-heading"]}
              {:fx/type :h-box :spacing 24 :alignment :center-left
               :children [{:fx/type board-size-field :label "Boards Wide" :ks [:game :map-width]}
                          {:fx/type board-size-field :label "Boards High" :ks [:game :map-height]}]}
              {:fx/type :label
               :text "Each board is one map section. Increase width or height to combine multiple boards."
               :style-class ["muted-label"]}
              {:fx/type :h-box :spacing 12 :alignment :center-left
               :children [{:fx/type board-selector}
                          {:fx/type :button :cursor :hand :text "LOAD SCENARIO"
                           :on-action {:event-type ::events/load-scenario :fx/sync true}}]}
              {:fx/type :label :text (or (fx/sub-val context get-in [:lobby :skirmish-launch-error]) "")
               :style-class ["skirmish-error"]}
              {:fx/type :h-box :alignment :center-right
               :children [{:fx/type :button :cursor :hand :text "BEGIN BATTLE"
                           :style-class ["skirmish-primary" "begin-battle-button" "skirmish-action-button"]
                           :style "-fx-padding: 10 24 10 24; -fx-min-width: 170px; -fx-min-height: 38px;"
                           :on-action {:event-type ::events/launch-game :fx/sync true}}]}]})

(defn skirmish-view [{:keys [fx/context]}]
  (let [forces (vec (take 2 (subs/forces context)))]
    {:fx/type :border-pane
     :style-class ["skirmish-root"]
     :top {:fx/type :h-box :alignment :center-left :spacing 12 :style-class ["skirmish-toolbar"]
           :children [{:fx/type :button :cursor :hand :text "BACK" :on-action {:event-type ::events/back-to-main-menu}}
                      {:fx/type :region :h-box/hgrow :always}
                      {:fx/type :label :text "SINGLEPLAYER / SKIRMISH SETUP" :style-class ["skirmish-breadcrumb"]}]}
     :center {:fx/type :v-box :spacing 15
              :children (concat
                         [{:fx/type :h-box :spacing 18
                           :children (mapv (fn [i f] {:fx/type force-panel :index i :force f})
                                           (range) forces)}]
                         [{:fx/type battlefield-panel}])}}))
