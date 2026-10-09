(ns megastrike.gui.views
  (:require
   [cljfx.api :as fx]
   [megastrike.gui.theme.theme :as theme]
   [megastrike.gui.elements :as elements]
   [megastrike.gui.events :as events]
   [megastrike.gui.lobby.views :as lobby]
   [megastrike.gui.lobby.skirmish :as skirmish]
   [megastrike.gui.subs :as subs]
   [megastrike.movement :as movement])
  (:import
   [javafx.scene.control Dialog DialogEvent]))

(defn attack-dialog
  [{:keys [fx/context]}]
  (let [props (subs/attack-dialog context)
        unit (:units props)
        attacks (:items props)
        phase (subs/phase context)
        active (subs/active-unit context)
        mv-type (if active (movement/selected-or-default active) :walk)]
    {:fx/type :dialog
     :showing (:showing props)
     :on-close-request (fn [^DialogEvent event]
                         (when (nil? (.getResult ^Dialog (.getSource event)))
                           (.consume event)))
     :header-text (str (:unit/full-name active) " attacking " (:unit/full-name unit))
     :on-hidden {:event-type ::events/close-attack-selection
                 :unit unit
                 :on-close {:event-type ::events/make-attack :unit unit}}
     :dialog-pane {:fx/type :dialog-pane
                   :button-types [:cancel]
                   :content {:fx/type :v-box
                             :spacing 5
                             :children (elements/attack-buttons attacks unit phase mv-type)}}}))

(defn round-dialog
  [{:keys [fx/context]}]
  (let [round (subs/turn-number context)
        phase (name (subs/phase context))
        round-report (subs/round-report context)
        props (subs/round-dialog context)]
    {:fx/type :dialog
     :showing (:showing props)
     :header-text (str "Turn " round " / " phase " phase")
     :on-hidden {:event-type ::events/close-round-dialog :phase-advance? false}
     :dialog-pane {:fx/type :dialog-pane
                   :button-types [:ok]
                   :content {:fx/type :scroll-pane
                             :content {:fx/type :text
                                       :text (or round-report "")}}}}))

(defn game-board
  [{:keys [fx/context]}]
  (let [gb (subs/tiles context)
        layout (subs/layout context)
        units (subs/units context)
        unit-locations (filter movement/deployed? units)
        destinations (filter #(pos? (count (:unit/path %))) (subs/units context))]
    {:fx/type :scroll-pane
     :content {:fx/type :group
               :children (concat
                          (for [h gb]
                            {:fx/type elements/draw-hex
                             :hex h
                             :layout layout})
                          (for [t unit-locations]
                            {:fx/type elements/draw-unit
                             :unit t
                             :layout layout})
                          (when (seq destinations)
                            (for [t destinations]
                              {:fx/type elements/draw-movement-path
                               :unit t
                               :layout layout})))}}))

(defn game-view [_]
  {:fx/type :grid-pane
   :children [{:fx/type game-board
               :grid-pane/row 0
               :grid-pane/column 0}
              {:fx/type elements/command-palette
               :grid-pane/row 1
               :grid-pane/column 0
               :grid-pane/column-span 2
               :grid-pane/hgrow :always
               :grid-pane/vgrow :always}
              {:fx/type elements/stat-blocks
               :grid-pane/row 0
               :grid-pane/column 1
               :grid-pane/hgrow :always
               :grid-pane/vgrow :always}]})
;; {:fx/type :stage
;;  :showing (subs/game-view context)
;;  :title (subs/title-string context)
;;  :scene {:fx/type :scene
;;            ;; :accelerators {[:minus] {:event-type ::events/change-size :direction :minus :fx/sync true}
;;            ;;                [:shift :equals] {:event-type ::events/change-size :direction :plus}}
;;          :root {:fx/type :grid-pane
;;                 :children [{:fx/type game-board
;;                             :grid-pane/row 0
;;                             :grid-pane/column 0}
;;                            {:fx/type elements/command-palette
;;                             :grid-pane/row 1
;;                             :grid-pane/column 0
;;                             :grid-pane/column-span 2
;;                             :grid-pane/hgrow :always
;;                             :grid-pane/vgrow :always}
;;                            {:fx/type elements/stat-blocks
;;                             :grid-pane/row 0
;;                             :grid-pane/column 1
;;                             :grid-pane/hgrow :always
;;                             :grid-pane/vgrow :always}]}}}

(defn lobby-view [_]
  {:fx/type skirmish/skirmish-view})

(defn menu-button [label event primary?]
  {:fx/type :button
   :text label
   :min-width 284
   :pref-height 44
   :style-class (if primary? ["menu-button" "primary-button"] ["menu-button"])
   :on-action {:event-type event}})

(defn main-menu-view [_]
  {:fx/type :stack-pane
   :style-class ["menu-root"]
   :alignment :center
   :children [{:fx/type :v-box
               :alignment :center
               :spacing 13
               :max-width 420
               :children [{:fx/type :label
                           :text "MEGASTRIKE"
                           :style-class ["menu-title"]}
                          {:fx/type :region :min-height 14}
                          (menu-button "SINGLEPLAYER" ::events/start-singleplayer true)
                          (menu-button "MULTIPLAYER" ::events/show-multiplayer false)
                          (menu-button "OPTIONS" ::events/show-options false)
                          (menu-button "QUIT" ::events/quit-application false)]}]})

(defn multiplayer-menu [{:keys [fx/context]}]
  {:fx/type :stack-pane
   :style-class ["menu-root"]
   :children [{:fx/type :v-box
               :alignment :center
               :spacing 12
               :max-width 380
               :children [{:fx/type :label :text "MULTIPLAYER" :style-class ["section-title"]}
                          {:fx/type elements/text-input :label "Player name" :ks [:client :player-id]}
                          {:fx/type :label :text "Hosting is available. Joining requires a separate network fix." :wrap-text true :style-class ["muted-label"]}
                          (menu-button "HOST GAME" ::events/host-game true)
                          (menu-button "BACK" ::events/back-to-main-menu false)]}]})

(defn options-menu [_]
  {:fx/type :stack-pane
   :style-class ["menu-root"]
   :children [{:fx/type :v-box
               :alignment :center
               :spacing 16
               :children [{:fx/type :label :text "OPTIONS" :style-class ["section-title"]}
                          {:fx/type :label :text "No configurable options are available yet." :style-class ["muted-label"]}
                          (menu-button "BACK" ::events/back-to-main-menu false)]}]})

(defn main-window-view [{:keys [fx/context] :as state}]
  (let [app-phase (fx/sub-val context :app-phase)]
    {:fx/type :stage
     :showing true
     :title "Megastrike"
     :width 1024
     :height 768
     ;; THE ROUTER: Swaps the scene content based on state
     :scene {:fx/type :scene
             :stylesheets [theme/stylesheet]
             :root (case app-phase
                     :main-menu {:fx/type main-menu-view}
                     :multiplayer {:fx/type multiplayer-menu}
                     :options {:fx/type options-menu}
                     :lobby     {:fx/type lobby-view}
                     :game      {:fx/type game-view}
                                                                   ;; Default fall-through
                     {:fx/type :v-box
                      :children [{:fx/type :label :text "Unknown State"}]})}}))

(defn root [{:keys [gui app-phase] :as state}]
  {:fx/type fx/ext-many
   :desc (concat
           ;; 1. The Main Application Window
          [{:fx/type main-window-view
            :app-phase app-phase}]

           ;; 2. Conditional Dialogs
           ;; Only render the component if the state says it is showing.
           ;; This prevents "hidden" windows from consuming resources.
          (when (get-in gui [:dialogs :attack-dialog :showing])
            [{:fx/type attack-dialog}])

          (when (get-in gui [:dialogs :round-dialog :showing])
            [{:fx/type round-dialog}]))})

