(ns megastrike.gui.events
  (:require
   [cljfx.api :as fx]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.brunobonacci.mulog :as mu]
   [megastrike.battle-force :as bf]
   [megastrike.board :as board]
   [megastrike.client :as client]
   [megastrike.combat-unit :as cu]
   [megastrike.gui.subs :as subs]
   [megastrike.logs :as logs]
   [megastrike.phases :as phases]
   [megastrike.scenario :as scenario]
   [megastrike.server.server :as server]
   [megastrike.turn-manager :as turn-manager]
   [megastrike.utils :as utils])
  (:import
   [javafx.application Platform]
   [javafx.event ActionEvent]
   [javafx.scene Node]
   [javafx.scene.control
    ButtonBar$ButtonData
    ButtonType
    Dialog
    DialogEvent]
   [javafx.scene.input MouseEvent]
   [javafx.stage FileChooser]))

;; Defaults and common operations
(defmulti event-handler :event-type)

(defmethod event-handler ::no-op
  [_])

(defmethod event-handler :default
  [{:keys [event-type] :as event}]
  (mu/log ::unhandled-event
          :event-type event-type
          :keys (keys event)))

(declare default-skirmish-forces)

;; Main menu navigation
(defmethod event-handler ::start-singleplayer
  [{:keys [fx/context]}]
  (let [player-id "Player 1"]
    {:context (fx/swap-context context (fn [state] (-> state (assoc-in [:client :player-id] player-id) (assoc :singleplayer? true))))
     :connect {:action :host :player-id player-id}}))

(defmethod event-handler ::show-multiplayer
  [{:keys [fx/context]}]
  {:context (fx/swap-context context assoc :app-phase :multiplayer)})

(defmethod event-handler ::show-options
  [{:keys [fx/context]}]
  {:context (fx/swap-context context assoc :app-phase :options)})

(defmethod event-handler ::back-to-main-menu
  [{:keys [fx/context]}]
  {:context (fx/swap-context context assoc :app-phase :main-menu)})

(defmethod event-handler ::quit-application
  [_]
  (Platform/exit))

;; Lobby Events

(def empty-game
  {:units nil :forces nil :current-phase :lobby :map-width "1" :map-height "1" :turn-number 0})

(defmethod event-handler ::select-camo
  [{:keys [^ActionEvent fx/context fx/event]}]
  (let [window (.getWindow (.getScene ^Node (.getTarget event)))
        chooser (doto (FileChooser.)
                  (.setTitle "Select Camo")
                  (.setInitialDirectory (io/file "data/images/camo")))]
    (when-let [camo (.showOpenDialog chooser window)]
      {:context (fx/swap-context context assoc-in [:gui :dialogs :force-creation-dialog :camo] (str "file:" (.getPath camo)))})))

;; TODO Switch to server-side work
(defmethod event-handler ::load-scenario
  [{:keys [^ActionEvent fx/context fx/event]}]
  (let [window (.getWindow (.getScene ^Node (.getTarget event)))
        chooser (doto (FileChooser.)
                  (.setTitle "Select Scenario")
                  (.setInitialDirectory (io/file "data/scenarios")))]
    (when-let [s (.showOpenDialog chooser window)]
      (let [conn (subs/connection context)
            player-id (subs/client-player context)
            scenario (slurp (io/file s))]
        (client/load-scenario-message conn scenario player-id))))
  {})

(defmethod event-handler ::load-mapboard
  [{:keys [^ActionEvent fx/context fx/event id]}]
  (let [window (.getWindow (.getScene ^Node (.getTarget event)))
        chooser (doto (FileChooser.)
                  (.setTitle "Select Mapboard")
                  (.setInitialDirectory (io/file "data/boards")))
        boards (subs/board context)]
    (when-let [board (.showOpenDialog chooser window)]
      {:context (fx/swap-context context assoc-in [:lobby :map-boards] (assoc boards id (board/create-mapsheet (str "file:" (.getPath board)))))})))

(defmethod event-handler ::filter-changed
  [{:keys [fx/context values]}]
  {:context (fx/swap-context context
                             (fn [state]
                               (-> state
                                   (assoc-in [:lobby :mul] (vec (cu/filter-units cu/mul values)))
                                   (assoc-in [:lobby :mul-category] values))))})

(defn positive-board-count [value]
  (try
    (let [n (Integer/parseInt (str value))]
      (when (<= 1 n 10) n))
    (catch Exception _ nil)))

(defn skirmish-launch-error [game boards]
  (let [forces (vec (:forces game))
        units (vec (:units game))
        width (positive-board-count (:map-width game))
        height (positive-board-count (:map-height game))]
    (cond
      (not= 2 (count forces)) "A skirmish requires two forces."
      (some #(str/blank? (:unit-group/name %)) forces) "Enter a name for each force."
      (not= 2 (count (distinct (map :unit-group/keyword forces)))) "Force identifiers must be different."
      (some #(not (#{:local-player :kevin} (:unit-group/player %))) forces) "Choose Human or Kevin (AI) for each force."
      (some #(not (contains? (set (map :unit/battle-force units)) (:unit-group/keyword %))) forces) "Add at least one unit to each force."
      (or (nil? width) (nil? height)) "Boards Wide and Boards High must be between 1 and 10."
      (not= (* width height) (count boards)) "Choose a map for every board position."
      (some nil? boards) "Choose a map for every board position."
      :else nil)))

(defmethod event-handler ::launch-game
  [{:keys [fx/context]}]
  (let [game (subs/game context)
        selected-boards (vec (or (subs/map-boards context) []))
        error (skirmish-launch-error game selected-boards)]
    (if error
      {:context (fx/swap-context context assoc-in [:lobby :skirmish-launch-error] error)}
      (let [width (positive-board-count (:map-width game))
            height (positive-board-count (:map-height game))
            next-game (-> game
                          (assoc :map-width width :map-height height)
                          (assoc :game-board (board/create-board selected-boards width height))
                          (phases/next-phase))
            gui (-> (subs/gui context)
                    (assoc :game-view true :lobby-view false))]
        ;; Singleplayer hosts the server in this same process. Keep its state in sync
        ;; with the match that the client is now displaying.
        (when (fx/sub-val context :singleplayer?)
          (reset! server/game-state next-game))
        {:context (fx/swap-context context (fn [state]
                    (-> state
                        (assoc :game next-game :gui gui :app-phase :game)
                        (assoc-in [:lobby :skirmish-launch-error] nil))))
         :dispatch {:event-type ::open-round-dialog}}))))

;; TODO Switch to server side handling
(defmethod event-handler ::load-save
  [{:keys [fx/context]}]
  (let [save-data (edn/read-string
                   (slurp (utils/load-resource :data "save.edn")))]
    {:context (fx/swap-context context merge save-data)}))

;; TODO Switch to Server side approval and medation
(defmethod event-handler ::change-player
  [{:keys [fx/context fx/event]}]
  {:context (fx/swap-context context assoc-in [:gui :dialogs :force-creation-dialog :player] event)})

;; TODO Switch to server-side handling
(defmethod event-handler ::add-force
  [{:keys [fx/context]}]
  (let [fname (subs/force-name context)
        deploy (subs/force-zone context)
        camo (subs/force-camo context)
        team (inc (count (subs/forces context)))
        player (subs/player-type context)
        new-force (bf/->battle-force fname deploy camo team player [])
        new-forces (bf/update-battle-force (subs/forces context) (:unit-group/keyword new-force) new-force)
        new-game (merge (subs/game context) {:forces new-forces})
        new-lobby (merge (subs/lobby context) {:force-camo nil})]
    {:context (fx/swap-context context assoc :game new-game :lobby new-lobby)
     :dispatch {:event-type ::close-dialog :dialog :force-creation-dialog}}))

(defmethod event-handler ::mul-selection-changed
  [{:keys [fx/context fx/event]}]
  {:context (fx/swap-context context assoc-in [:lobby :active-mul] event)})

;; TODO Switch to server side handling
(defmethod event-handler ::add-unit
  [{:keys [fx/context]}]
  (let [pilot {:name (subs/p-name context)
               :skill (subs/p-skill context)}
        units (cu/->combat-unit
               {:units (subs/units context)
                :mul-unit (subs/active-mul context)
                :pilot pilot
                :battle-force (subs/lobby-active-force context)})]
    {:context
     (fx/swap-context context assoc-in [:game :units] units)
     :dispatch {:event-type ::close-dialog :dialog :mul-dialog}}))

(defmethod event-handler ::filter-mul
  [{:keys [fx/context field]}]
  (let [term (subs/mul-search-term context)]
    {:context (fx/swap-context context assoc-in [:lobby :mul] (cu/filter-units cu/mul field term str/includes?))}))

(defmethod event-handler ::force-selection-changed
  [{:keys [fx/context fx/event]}]
  (let [new-lobby (merge (subs/lobby context)
                         {:active-force (:unit-group/keyword event)
                          :force-zone (str (:unit-group/deployment event))
                          :force-name (:unit-group/name event)
                          :force-camo (:unit-group/camo event)})]
    {:context (fx/swap-context context assoc :lobby new-lobby)}))

(defmethod event-handler ::unit-selection-changed
  [{:keys [fx/context fx/event]}]
  {:context (fx/swap-context context assoc-in [:gui :active-unit] (:id event))})

;; Singleplayer skirmish configuration
(def deployment-options ["N" "NE" "E" "SE" "S" "SW" "W" "NW" "EDG" "CTR"])

(defn default-skirmish-forces []
  [(bf/->battle-force "Your Force" "S" nil 1 :local-player [])
   (bf/->battle-force "Enemy Force" "N" nil 2 :kevin [])])

(defmethod event-handler ::skirmish-change-force
  [{:keys [fx/context force-id field fx/event]}]
  (let [forces (vec (or (subs/forces context) (default-skirmish-forces)))
        index (.indexOf (mapv :unit-group/keyword forces) force-id)
        value (case field
                :unit-group/deployment (keyword "direction" (str/lower-case (str event)))
                :unit-group/player (if (= event "Kevin (AI)") :kevin :local-player)
                event)]
    (when (>= index 0)
      {:context (fx/swap-context context assoc-in [:game :forces index field] value)})))

(defmethod event-handler ::skirmish-select-force
  [{:keys [fx/context force-id]}]
  {:context (fx/swap-context context assoc-in [:lobby :active-force] force-id)})

(defmethod event-handler ::skirmish-remove-unit
  [{:keys [fx/context unit-id]}]
  {:context (fx/swap-context context update-in [:game :units]
                            (fn [units] (vec (remove #(= (:unit/id %) unit-id) units))))})

(defmethod event-handler ::skirmish-select-unit
  [{:keys [fx/context fx/event force-index]}]
  {:context (fx/swap-context context assoc-in [:lobby :skirmish-selected force-index] (:unit/id event))})

(defmethod event-handler ::skirmish-add-unit
  [{:keys [fx/context force-id]}]
  (let [selected (subs/active-mul context)
        skill (try (Integer/parseInt (str (or (subs/p-skill context) "4")))
                   (catch Exception _ 4))
        pilot {:pilot/full-name (or (subs/p-name context) "Pilot")
               :pilot/skill skill :pilot/kills 0}]
    (when selected
      {:context (fx/swap-context context update-in [:game :units]
                                (fn [units] (cu/->combat-unit {:units units :mul-unit selected
                                                               :pilot pilot :battle-force force-id})))})))

(defmethod event-handler ::skirmish-open-unit-dialog
  [{:keys [fx/context force-id]}]
  {:context (fx/swap-context context (fn [state] (-> state
    (assoc-in [:lobby :active-force] force-id)
    (assoc-in [:gui :dialogs :mul-dialog :showing] true))))})

;; Client only events

(defmethod event-handler ::host-game
  [{:keys [fx/context]}]
  {:connect {:action :host
             :player-id (:player-id (subs/client context))}})

(defmethod event-handler ::connection-established
  [{:keys [fx/context connection player-id]}]
  (let [client (subs/client context)
        new-client (-> client
                       (assoc :connection connection)
                       (assoc :connection-status :connected)
                       (assoc :player-id (utils/keyword-maker player-id)))]
    (client/message-server connection {:action :join-server :player-id player-id})
    {:context (fx/swap-context context (fn [state]
                (cond-> (assoc state :app-phase :lobby :client new-client)
                  (and (:singleplayer? state) (empty? (get-in state [:game :forces])))
                  (assoc-in [:game :forces] (default-skirmish-forces)))))}))

(defmethod event-handler :server-update-received
  [{:keys [fx/context game]}]
  (mu/log ::message-received
          :game-state (keys game))
  {:context (fx/swap-context context (fn [state]
    (let [local-game (:game state)]
      (assoc state :game (if (:singleplayer? state)
                           (-> game
                               (assoc :forces (or (:forces local-game) (default-skirmish-forces)))
                               (assoc :units (or (:units local-game) (:units game))))
                           game)))))})

(defmethod event-handler ::show-confirmation
  [{:keys [fx/context dialog-id]}]
  {:context (fx/swap-context context assoc-in [:gui :dialogs dialog-id :showing] true)})

(defmethod event-handler ::on-confirmation-dialog-hidden
  [{:keys [fx/context ^DialogEvent fx/event dialog-id on-confirmed]}]
  (condp = (.getButtonData ^ButtonType (.getResult ^Dialog (.getSource event)))
    ButtonBar$ButtonData/CANCEL_CLOSE
    {:context (fx/swap-context context assoc-in [:gui :dialogs dialog-id :showing] false)}

    ButtonBar$ButtonData/OK_DONE
    {:context (fx/swap-context context assoc-in [:gui :dialogs dialog-id :showing] false)
     :dispatch on-confirmed}))

(defmethod event-handler ::close-dialog
  [{:keys [fx/context dialog]}]
  {:context (fx/swap-context context assoc-in [:gui :dialogs dialog :showing] false)})

;; board events
(defmethod event-handler ::hex-clicked
  [{:keys [fx/context hex fx/event]}]
  (let [e ^MouseEvent event
        click-location {:x (.getX e) :y (.getY e)}
        active-unit (subs/active-id context)]
    {:context (fx/swap-context context assoc :game
                               (turn-manager/hex-clicked (subs/game context)
                                                         (subs/layout context)
                                                         hex click-location active-unit))}))

(defmethod event-handler ::text-input
  [{:keys [fx/context ks fx/event]}]
  {:context (fx/swap-context context assoc-in ks event)})

(defmethod event-handler ::change-size
  [{:keys [fx/context direction]}]
  (let [layout (subs/layout context)
        new-layout (if (= direction :plus)
                     (assoc layout :scale (+ (:scale layout) 0.1))
                     (assoc layout :scale (- (:scale layout) 0.1)))]
    {:context (fx/swap-context context assoc-in [:gui :layout] new-layout)}))

;; Saving, loading, and Phases
(defmethod event-handler ::auto-save
  [{:keys [fx/context]}]
  (prn (keys (subs/game context)))
  (spit "test-data.edn" (scenario/edn-scenario-writer (subs/game context))))

(defmethod event-handler ::quit-game
  [{:keys [fx/context]}]
  (logs/logs)
  (when (:connection (subs/connection context))
    (client/close-server (subs/connection context)))
  (Platform/exit))

(defmethod event-handler ::open-round-dialog
  [{:keys [fx/context]}]
  (let [ctx (get-in context [:dialogs :round-dialog])
        advance-phase? (empty? (subs/turn-order context))]
    {:context (fx/swap-context context assoc-in [:dialogs :round-dialog]
                               (assoc ctx :showing true :advance-phase? advance-phase?))}))

(defmethod event-handler ::close-round-dialog
  [{:keys [fx/context]}]
  {:context (fx/swap-context context assoc-in [:dialogs :round-dialog]
                             {:showing false :advance-phase? false})})

;; Unit selection
(defmethod event-handler ::stats-clicked
  [{:keys [fx/context unit]}]
  {:context (fx/swap-context context assoc :gui
                             (turn-manager/switch-unit (subs/game context) (subs/gui context) unit))})

(defmethod event-handler ::unit-clicked
  [{:keys [fx/context unit]}]
  (let [old-state (subs/game context)
        old-gui (subs/gui context)
        response (turn-manager/unit-clicked old-state old-gui unit)
        new-state (:game response)
        new-gui (:gui response)]
    {:context (fx/swap-context context assoc :game new-state :gui new-gui)}))

(defmethod event-handler ::undeploy-unit
  [{:keys [fx/context]}]
  {:context (fx/swap-context context assoc :game
                             (turn-manager/undeploy-unit (subs/game context)
                                                         (subs/gui context)))})

;; Movement Phase

(defmethod event-handler ::turn-button-clicked
  [{:keys [fx/context]}]
  {:context (fx/swap-context context update-in [:game :turn-flag] not)})

(defmethod event-handler ::set-movement-mode
  [{:keys [fx/context unit mode]}]
  {:context (fx/swap-context context assoc :game
                             (turn-manager/set-movement-mode (subs/game context) unit mode))})

(defmethod event-handler ::cancel-move
  [{:keys [fx/context unit]}]
  {:context (fx/swap-context context assoc :game
                             (turn-manager/cancel-move (subs/game context) unit))})

;; Combat Phase
(defmethod event-handler ::set-attack
  [{:keys [fx/context targeting]}]
  {:context (fx/swap-context context assoc :game
                             (turn-manager/set-special-attack (subs/game context)
                                                              (subs/gui context)
                                                              targeting))
   :dispatch {:event-type ::next-phase}})

(defmethod event-handler ::close-attack-selection
  [{:keys [fx/context selected]}]
  (let [ctx (get-in context [:gui :dialogs :attack-dialog])]
    (cond
      (= (:targeting/attack-type selected) nil)
      {:context (fx/swap-context context assoc-in [:gui :dialogs :attack-dialog]
                                 (assoc ctx :showing false :items []))}

      (contains? #{:attack/charge :attack/dfa} (:attack selected))
      {:context (fx/swap-context context assoc-in [:gui :dialogs :attack-dialog]
                                 (assoc ctx :showing false :items []))
       :dispatch {:event-type ::set-attack :targeting selected}}

      :else
      {:context (fx/swap-context context assoc-in [:gui :dialogs :attack-dialog]
                                 (assoc ctx :showing false :items []))
       :dispatch {:event-type ::make-attack :targeting selected}})))

;; Client/Server events

(defmethod event-handler ::connect-server
  [{:keys [fx/context address port]}]
  {:context (fx/swap-context context assoc-in [:client :connection] (client/create-connection address port))})

(defmethod event-handler ::close-server
  [{:keys [fx/context]}]
  (client/close-server (subs/connection context)))

(defmethod event-handler ::message-server
  [{:keys [fx/context]}]
  (prn (client/message-server (subs/connection context) {:action :message :text "ping"}))
  {})

(defmethod event-handler :server-message
  [{:keys [fx/context game]}]
  (prn game))

;; Server Events

(defmethod event-handler ::next-phase
  [{:keys [fx/context]}]
  {:context (fx/swap-context context assoc :game
                             (turn-manager/advance-turn (subs/game context)
                                                        (subs/gui context)
                                                        (subs/layout context)))
   :dispatch {:event-type ::open-round-dialog}})

;; Deployment Phase
(defmethod event-handler ::deploy-unit
  [{:keys [fx/context]}]
  (let [active-unit (subs/active-unit context)
        conn (subs/connection context)
        player (subs/client-player context)]
    (client/deploy-unit-message conn player active-unit))
  {:context (fx/swap-context context assoc :game
                             (turn-manager/deploy-unit (subs/game context)
                                                       (subs/gui context)))})

(defmethod event-handler ::confirm-move
  [{:keys [fx/context]}]
  {:context (fx/swap-context context assoc :game
                             (turn-manager/confirm-move (subs/game context)
                                                        (subs/gui context)
                                                        (subs/layout context)))})

(defmethod event-handler ::finish-attacks
  [{:keys [fx/context]}]
  {:context (fx/swap-context context assoc :game
                             (turn-manager/advance-turn (subs/game context)
                                                        (subs/gui context)
                                                        (subs/layout context)))})

(defmethod event-handler ::make-attack
  [{:keys [fx/context targeting]}]
  {:context (fx/swap-context context assoc :game
                             (turn-manager/make-attack (subs/game context) targeting))
   :dispatch {:event-type ::open-round-dialog}})

(defmethod event-handler ::resolve-physicals
  [{:keys [fx/context]}]
  {:context (fx/swap-context context assoc :game
                             (turn-manager/resolve-physical-attacks (subs/game context) (subs/layout context)))
   :dispatch {:event-type ::open-round-dialog}})

