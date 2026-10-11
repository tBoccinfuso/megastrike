(ns megastrike.gui.views
  (:require
   [cljfx.api :as fx]
   [megastrike.gui.theme.theme :as theme]
   [megastrike.gui.elements :as elements]
   [megastrike.gui.events :as events]
   [megastrike.gui.lobby.views :as lobby]
   [megastrike.gui.lobby.skirmish :as skirmish]
   [megastrike.gui.subs :as subs]
   [megastrike.battle-force :as battle-force]
   [megastrike.movement :as movement]
   [megastrike.hexagons.hex :as hex])
  (:import
   [javafx.scene.control Dialog DialogEvent ScrollPane]
   [javafx.scene.input MouseButton MouseEvent]))

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

(defonce map-pan-origin (atom nil))
(defonce wheel-last-zoom (atom 0))
(defonce ^:private static-map-cache (atom nil))

(defn static-map-layer
  "Cache the terrain Canvas and hex hit-target descriptions until board or
   layout changes. Hover, selection and phase updates reuse the identical
   scene descriptions. One-entry cache avoids retaining earlier zoom levels."
  [tiles layout]
  (let [old @static-map-cache]
    (if (and (identical? tiles (:tiles old)) (= layout (:layout old)))
      (:nodes old)
      (let [nodes (into [{:fx/type elements/terrain-canvas
                          :tiles tiles
                          :layout layout}]
                        (map (fn [h] {:fx/type elements/hex-hit-target
                                      :hex h
                                      :layout layout}) tiles))]
        (reset! static-map-cache {:tiles tiles :layout layout :nodes nodes})
        nodes))))


(defn game-board
  [{:keys [fx/context]}]
  (let [gb (subs/tiles context)
        layout (subs/layout context)
        units (subs/units context)
        unit-locations (filter movement/deployed? units)
        destinations (filter #(pos? (count (:unit/path %))) (subs/units context))]
    {:fx/type :scroll-pane
     :style-class ["battle-map-scroll"]
     :focus-traversable true
     :pannable false
     :fit-to-width true
     :fit-to-height true
     :hvalue 0.5
     :vvalue 0.5
     :hbar-policy :never
     :vbar-policy :never
     :on-scroll {:event-type ::events/battle-map-scroll :fx/sync true}
     :on-mouse-pressed (fn [^MouseEvent event]
                         (when (= MouseButton/MIDDLE (.getButton event))
                           (let [pane ^ScrollPane (.getCurrentTarget event)]
                             (reset! map-pan-origin [(.getSceneX event) (.getSceneY event)
                                                     (.getHvalue pane) (.getVvalue pane)]))
                           (.consume event)))
     :on-mouse-dragged (fn [^MouseEvent event]
                         (when (and (.isMiddleButtonDown event) @map-pan-origin)
                           (let [pane ^ScrollPane (.getCurrentTarget event)
                                 [x y h v] @map-pan-origin
                                 bounds (.getLayoutBounds (.getContent pane))
                                 viewport (.getViewportBounds pane)
                                 dx (max 1.0 (- (.getWidth bounds) (.getWidth viewport)))
                                 dy (max 1.0 (- (.getHeight bounds) (.getHeight viewport)))]
                             (.setHvalue pane (-> (- h (/ (- (.getSceneX event) x) dx)) (max 0.0) (min 1.0)))
                             (.setVvalue pane (-> (- v (/ (- (.getSceneY event) y) dy)) (max 0.0) (min 1.0))))
                           (.consume event)))
     :on-mouse-released (fn [^MouseEvent event]
                          (when (= MouseButton/MIDDLE (.getButton event))
                            (reset! map-pan-origin nil)
                            (.consume event)))
     :content {:fx/type :stack-pane
               :on-scroll {:event-type ::events/battle-map-scroll :fx/sync true}
               :alignment :center
               :children [{:fx/type :group
                           :on-scroll {:event-type ::events/battle-map-scroll :fx/sync true}
                           :children (concat
                          (static-map-layer gb layout)
                          (for [t unit-locations]
                            {:fx/type elements/draw-unit
                             :unit t
                             :layout layout})
                          (when (seq destinations)
                            (for [t destinations]
                              {:fx/type elements/draw-movement-path
                               :unit t
                               :layout layout}))) }]}}))

(defn battle-stat [label value]
  {:fx/type :h-box
   :alignment :center-left
   :children [{:fx/type :label :text label :style-class ["battle-muted"]}
              {:fx/type :region :h-box/hgrow :always}
              {:fx/type :label :text (str (or value "—")) :style-class ["battle-stat-value"]}]})

(defn battle-unit-row [{:keys [unit active]}]
  (let [selected? (= (:unit/id unit) active)
        armor (get-in unit [:unit/armor :toughness/current] 0)
        structure (get-in unit [:unit/structure :toughness/current] 0)]
    {:fx/type :h-box
     :alignment :center-left
     :spacing 9
     :style-class (if selected? ["battle-roster-row" "battle-roster-selected"] ["battle-roster-row"])
     :on-mouse-clicked {:event-type ::events/stats-clicked :fx/sync true :unit (:unit/id unit)}
     :children [{:fx/type :label :text (str (:unit/id unit)) :min-width 0 :pref-width 36 :style-class ["battle-roster-id"] :style "-fx-text-fill: #a1c9ed; -fx-font-weight: bold;"}
                {:fx/type :label :text (str (or (:unit/full-name unit) (:unit/id unit)))
                 :max-width Double/MAX_VALUE :h-box/hgrow :always :style-class ["battle-roster-name"] :style "-fx-text-fill: #e5e9eb; -fx-font-size: 12px;"}
                {:fx/type :label :text (str armor " / " structure) :style-class ["battle-roster-health"] :style "-fx-text-fill: #d0d8dc;"}]}))

(defn battle-sidebar [{:keys [fx/context]}]
  (let [units (subs/units context)
        active (subs/active-id context)
        selected (subs/active-unit context)
        forces (subs/forces context)
        groups (group-by :unit/battle-force units)]
    {:fx/type :scroll-pane
     :fit-to-width true
     :hbar-policy :never
     :style-class ["battle-sidebar-scroll"]
     :content {:fx/type :v-box
               :spacing 10
               :style-class ["battle-sidebar"]
               :children [{:fx/type :v-box
                           :spacing 8
                           :style-class ["battle-panel"]
                           :children (if selected
                                       [{:fx/type :label :text "Selected Unit" :style-class ["battle-panel-title"]}
                                        {:fx/type :label :text (str (or (:unit/full-name selected) (:unit/id selected))) :wrap-text true :style-class ["battle-selected-name"]}
                                        {:fx/type :label :text (str "Pilot: " (or (get-in selected [:unit/pilot :pilot/name]) "—")) :style-class ["battle-muted"]}
                                        {:fx/type :separator}
                                        (battle-stat "Armor" (str (get-in selected [:unit/armor :toughness/current] 0) " / " (get-in selected [:unit/armor :toughness/maximum] 0)))
                                        (battle-stat "Structure" (str (get-in selected [:unit/structure :toughness/current] 0) " / " (get-in selected [:unit/structure :toughness/maximum] 0)))
                                        (battle-stat "Heat" (:unit/current-heat selected))
                                        {:fx/type :separator}
                                        (battle-stat "Movement" (movement/print-movement selected))
                                        (battle-stat "TMM" (movement/base-tmm selected))]
                                       [{:fx/type :label :text "Selected Unit" :style-class ["battle-panel-title"]}
                                        {:fx/type :label :text "Select a unit from the roster or the battlefield." :wrap-text true :style-class ["battle-muted"]}])}
                          {:fx/type :v-box
                           :spacing 4
                           :style-class ["battle-panel" "battle-hex-info"]
                           :children (let [h (fx/sub-val context get-in [:gui :hovered-hex])]
                                       (if h
                                         (let [offset (hex/hex->offset h)]
                                           [{:fx/type :label :text "Hex Details" :style-class ["battle-panel-title"]}
                                            (battle-stat "Coordinates" (format "%02d%02d" (:x offset) (:y offset)))
                                            (battle-stat "Terrain" (or (:terrain h) "Clear"))
                                            (battle-stat "Elevation" (:elevation h))])
                                         [{:fx/type :label :text "Hex Details" :style-class ["battle-panel-title"]}
                                          {:fx/type :label :text "Hover a hex or unit to inspect terrain." :wrap-text true :style-class ["battle-muted"]}]))}
                          {:fx/type :v-box
                           :spacing 7
                           :style-class ["battle-panel"]
                           :children (into [{:fx/type :label :text "Force Roster" :style-class ["battle-panel-title"]}]
                                           (mapcat (fn [[force-id force-units]]
                                                     (let [force (battle-force/select-force forces force-id)]
                                                       (cons {:fx/type :label
                                                              :text (str (or (:unit-group/name force) (str force-id)) " (" (count force-units) ")")
                                                              :style-class ["battle-force-title"]}
                                                             (map (fn [u] {:fx/type battle-unit-row :unit u :active active}) force-units))))
                                                   groups))}]}}))

(defn battle-top-bar [{:keys [fx/context]}]
  (let [phase (subs/phase context)
        turn (subs/turn-number context)
        force (subs/active-force context)]
    {:fx/type :h-box
     :spacing 14
     :alignment :center-left
     :style-class ["battle-top-bar"]
     :children [{:fx/type :label :text "MEGASTRIKE" :style-class ["battle-top-brand"]}
                {:fx/type :separator :orientation :vertical}
                {:fx/type :label :text (str "Turn " (or turn 1)) :style-class ["battle-top-accent"]}
                {:fx/type :separator :orientation :vertical}
                {:fx/type :label :text (str (if phase (clojure.string/capitalize (name phase)) "Unknown") " Phase") :style-class ["battle-top-text"]}
                {:fx/type :separator :orientation :vertical}
                {:fx/type :label :text (str "Active: " (or (:unit-group/name force) "—")) :style-class ["battle-top-text"]}
                {:fx/type :region :h-box/hgrow :always}
                {:fx/type :button :text "−" :style-class ["battle-utility-button"] :style "-fx-background-color: #2b3236; -fx-text-fill: #f1f3f4; -fx-border-color: #58646b; -fx-border-width: 1; -fx-background-radius: 0; -fx-border-radius: 0; -fx-padding: 9 16; -fx-min-height: 36px; -fx-cursor: hand;"
                 :on-action {:event-type ::events/change-size :direction :minus :fx/sync true}}
                {:fx/type :button :text "+" :style-class ["battle-utility-button"] :style "-fx-background-color: #2b3236; -fx-text-fill: #f1f3f4; -fx-border-color: #58646b; -fx-border-width: 1; -fx-background-radius: 0; -fx-border-radius: 0; -fx-padding: 9 16; -fx-min-height: 36px; -fx-cursor: hand;"
                 :on-action {:event-type ::events/change-size :direction :plus :fx/sync true}}]}))

(defn battle-action [label event disabled?]
  {:fx/type :button
   :text label
   :disable (boolean disabled?)
   :style-class ["battle-action-button"]
   :style "-fx-background-color: #2b3236; -fx-text-fill: #f1f3f4; -fx-border-color: #58646b; -fx-border-width: 1; -fx-background-radius: 0; -fx-border-radius: 0; -fx-padding: 9 16; -fx-min-height: 36px; -fx-cursor: hand;"
   :on-action event})

(defn phase-guidance [phase selected?]
  (case phase
    :initiative ["NEXT STEP" "Click Continue to Deployment to start placing your forces on the map."]
    :deployment ["DEPLOYMENT" (if selected?
                                 "Now click a legal map hex, then choose Deploy Unit below."
                                 "1. Select a unit from the roster.  2. Click a deployment hex.  3. Deploy Unit.")]
    :movement ["MOVEMENT" (if selected?
                             "Choose Walk or Jump, select a destination, then confirm movement."
                             "Select one of your deployed units from the roster or map.")]
    :combat ["COMBAT" "Select an attacking unit and a target; review and confirm the attack."]
    ["NEXT STEP" "Use the controls below to complete the current phase."]))

(defn battle-command-bar [{:keys [fx/context]}]
  (let [phase (subs/phase context)
        unit (subs/active-unit context)
        turn-order (subs/turn-order context)
        [step message] (phase-guidance phase (boolean unit))
        phase-buttons (cond
                        (= phase :deployment) (elements/deploy-buttons (empty? turn-order))
                        (= phase :movement) (if unit (elements/move-buttons unit) [])
                        (= phase :combat) elements/combat-phase-buttons
                        :else [])
        primary (assoc (battle-action (if (= phase :initiative) "Continue to Deployment" "Next Phase")
                                      {:event-type ::events/next-phase}
                                      (not (empty? turn-order)))
                       :style-class ["battle-primary-button"] :style "-fx-background-color: #41311f; -fx-text-fill: #fff0d5; -fx-border-color: #c68a3c; -fx-border-width: 1; -fx-background-radius: 0; -fx-border-radius: 0; -fx-padding: 10 18; -fx-min-height: 42px; -fx-min-width: 190px; -fx-font-weight: bold; -fx-cursor: hand;")
        secondary (vec (concat (map #(assoc % :style-class ["battle-phase-button"] :style "-fx-background-color: #2b3236; -fx-text-fill: #f1f3f4; -fx-border-color: #58646b; -fx-border-width: 1; -fx-background-radius: 0; -fx-border-radius: 0; -fx-padding: 9 16; -fx-min-height: 36px; -fx-cursor: hand;") phase-buttons)
                               (when (seq phase-buttons) [{:fx/type :separator :orientation :vertical}])
                               [(battle-action "Round Report" {:event-type ::events/open-round-dialog} false)
                                (battle-action "Save Game" {:event-type ::events/auto-save :fx/sync true} false)
                                (battle-action "Exit" {:event-type ::events/quit-game} false)]))]
    {:fx/type :v-box
     :style-class ["battle-command-area"]
     :children [{:fx/type :h-box
                 :alignment :center-left
                 :spacing 12
                 :style-class ["battle-guidance"]
                 :children [{:fx/type :label :text step :style-class ["battle-guidance-step"]}
                            {:fx/type :label :text message :wrap-text true :style-class ["battle-guidance-message"]}]}
                {:fx/type :h-box
                 :alignment :center-left
                 :spacing 8
                 :style-class ["battle-command-row"]
                 :children [{:fx/type :scroll-pane
                             :fit-to-height true
                             :hbar-policy :as-needed
                             :vbar-policy :never
                             :h-box/hgrow :always
                             :style-class ["battle-command-scroll"]
                             :content {:fx/type :h-box
                                       :alignment :center-left
                                       :spacing 8
                                       :style-class ["battle-command-bar"]
                                       :children secondary}}
                            primary]}]}))

(defn game-view [_]
  {:fx/type :border-pane
   :style-class ["battle-root"]
   :top {:fx/type battle-top-bar}
   :center {:fx/type game-board}
   :right {:fx/type battle-sidebar}
   :bottom {:fx/type battle-command-bar}})

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
     :maximized (contains? #{:lobby :battle-loading :game} app-phase)
     :full-screen false
     ;; THE ROUTER: Swaps the scene content based on state
     :scene (cond-> {:fx/type :scene
             :stylesheets [theme/stylesheet]
             :root (case app-phase
                     :battle-loading {:fx/type :stack-pane
                                      :style-class ["menu-root"]
                                      :children [{:fx/type :v-box
                                                  :alignment :center
                                                  :spacing 18
                                                  :children [{:fx/type :label :text "PREPARING BATTLE" :style-class ["section-title"]}
                                                             {:fx/type :progress-indicator :pref-width 42 :pref-height 42}
                                                             {:fx/type :label :text "Building map and initializing forces..." :style-class ["muted-label"]}]}]}
                     :connecting {:fx/type :stack-pane
                                  :style-class ["menu-root"]
                                  :children [{:fx/type :v-box
                                              :alignment :center
                                              :spacing 16
                                              :children [{:fx/type :progress-indicator}
                                                         {:fx/type :label :text "Preparing singleplayer..." :style-class ["muted-label"]}]}]}
                     :main-menu {:fx/type main-menu-view}
                     :multiplayer {:fx/type multiplayer-menu}
                     :options {:fx/type options-menu}
                     :lobby     {:fx/type lobby-view}
                     :game      {:fx/type game-view}
                                                                   ;; Default fall-through
                     {:fx/type :v-box
                      :children [{:fx/type :label :text "Unknown State"}]})}
             (= app-phase :game) (assoc :on-key-pressed {:event-type ::events/battle-key-pressed :fx/sync true}))}))

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

