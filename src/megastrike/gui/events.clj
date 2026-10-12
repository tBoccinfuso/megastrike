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
   [megastrike.deployment :as deployment]
   [megastrike.strategic-deployment :as strategic-deployment]
   [megastrike.gui.subs :as subs]
   [megastrike.logs :as logs]
   [megastrike.movement :as movement]
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
   [javafx.scene.input MouseEvent ScrollEvent KeyEvent KeyCode]
   [javafx.scene.control ScrollPane]
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

;; Shared skirmish deployment-zone depth. Keep before all event handlers.
(def deployment-zone-depth 3)

;; Main menu navigation
(defmethod event-handler ::start-singleplayer
  [{:keys [fx/context]}]
  (let [player-id "Player 1"]
    {:context (fx/swap-context context (fn [state] (-> state (assoc-in [:client :player-id] player-id) (assoc :singleplayer? true :app-phase :connecting))))
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

(declare positive-board-count)

(defn available-mapboards []
  (->> (file-seq (io/file "data/boards"))
       (filter #(.isFile ^java.io.File %))
       (filter #(str/ends-with? (str/lower-case (.getName ^java.io.File %)) ".board"))
       (sort-by #(.getPath ^java.io.File %))
       vec))

(defmethod event-handler ::randomize-mapboards
  [{:keys [fx/context]}]
  (let [choices (available-mapboards)
        width (positive-board-count (subs/map-width context))
        height (positive-board-count (subs/map-height context))]
    (cond
      (or (nil? width) (nil? height))
      {:context (fx/swap-context context assoc-in [:lobby :skirmish-launch-error]
                                 "Set Boards Wide and Boards High to values between 1 and 10.")}
      (empty? choices)
      {:context (fx/swap-context context assoc-in [:lobby :skirmish-launch-error]
                                 "No .board files found in data/boards.")}
      :else
      (let [chosen (take (* width height) (cycle (shuffle choices)))
            mapsheets (mapv #(board/create-mapsheet (.getPath ^java.io.File %)) chosen)]
        {:context (fx/swap-context context
                                   (fn [state]
                                     (-> state
                                         (assoc-in [:lobby :map-boards] mapsheets)
                                         (assoc-in [:lobby :skirmish-launch-error] nil))))}))))

(defmethod event-handler ::set-skirmish-option
  [{:keys [fx/context option fx/event]}]
  {:context (fx/swap-context context assoc-in [:lobby :options option] (boolean event))})

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
        random? (true? (fx/sub-val context get-in [:lobby :options :random-map]))
        width (positive-board-count (:map-width game))
        height (positive-board-count (:map-height game))
        choices (when random? (available-mapboards))
        selected-boards (if (and random? width height (seq choices))
                          (mapv #(board/create-mapsheet (.getPath ^java.io.File %))
                                (take (* width height) (cycle (shuffle choices))))
                          (vec (or (subs/map-boards context) [])))
        error (if (and random? (empty? choices))
                "No .board files found for Random Map."
                (skirmish-launch-error game selected-boards))]
    (if error
      {:context (fx/swap-context context assoc-in [:lobby :skirmish-launch-error] error)}
      (let [width (positive-board-count (:map-width game))
            height (positive-board-count (:map-height game))]
        {:context (fx/swap-context context
                                   (fn [state] (-> state
                                                   (assoc :app-phase :battle-loading)
                                                   (assoc-in [:lobby :map-boards] selected-boards))))
         :prepare-battle {:game game
                          :boards selected-boards
                          :width width
                          :height height}}))))

(defmethod event-handler ::battle-ready
  [{:keys [fx/context game]}]
  (let [auto? (and (fx/sub-val context :singleplayer?)
                   (true? (fx/sub-val context get-in [:lobby :options :smart-auto-deploy])))
        result (when auto? (strategic-deployment/auto-deploy-from-initiative game deployment-zone-depth))
        next-game (if result (:game result) game)
        problem (when (and result (seq (:unplaced result)))
                  (str "Could not automatically place " (count (:unplaced result))
                       " units. Complete deployment manually."))]
    (when (fx/sub-val context :singleplayer?)
      (reset! server/game-state next-game))
    {:context (fx/swap-context context
                               (fn [state]
                                 (-> state
                                     (assoc :game next-game :app-phase :game)
                                     (assoc-in [:gui :layout :scale] 0.65)
                                     (assoc-in [:gui :deployment-error] problem)
                                     (assoc-in [:lobby :skirmish-launch-error] nil))))
     :dispatch {:event-type ::open-round-dialog}}))

(defmethod event-handler ::battle-load-failed
  [{:keys [fx/context message]}]
  {:context (fx/swap-context context
                             (fn [state]
                               (-> state
                                   (assoc :app-phase :lobby)
                                   (assoc-in [:lobby :skirmish-launch-error]
                                             (str "Battle initialization failed: " message)))))})

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
(defonce last-hex-hover-update (atom 0))

(defmethod event-handler ::hex-hovered
  [{:keys [fx/context hex]}]
  ;; Avoid re-rendering thousands of hex nodes on every pointer crossing.
  (let [previous (fx/sub-val context get-in [:gui :hovered-hex])
        now (System/nanoTime)]
    (when (and (not= previous hex)
               (> (- now @last-hex-hover-update) 100000000))
      (reset! last-hex-hover-update now)
      {:context (fx/swap-context context assoc-in [:gui :hovered-hex] hex)})))

(defn deployment-placement-result
  "Validate a hex before the normal placement handler stages a location."
  [game unit hex]
  (cond
    (nil? unit) "Select an undeployed unit from the roster first."
    (:unit/acted? unit) "That unit has already deployed."
    (not (deployment/eligible-location? game unit hex deployment-zone-depth))
    "Choose an unoccupied hex in this force's highlighted deployment zone."
    :else nil))

(defmethod event-handler ::hex-clicked
  [{:keys [fx/context hex fx/event]}]
  (let [e ^MouseEvent event
        click-location {:x (.getX e) :y (.getY e)}
        active-id (subs/active-id context)
        game (subs/game context)
        unit (cu/select-unit (:units game) active-id)
        problem (when (= :deployment (:current-phase game))
                  (deployment-placement-result game unit hex))]
    (if problem
      {:context (fx/swap-context context assoc-in [:gui :deployment-error] problem)}
      {:context (fx/swap-context context
                                 (fn [state]
                                   (-> state
                                       (assoc-in [:gui :deployment-error] nil)
                                       (assoc :game (turn-manager/hex-clicked game
                                                                              (subs/layout context)
                                                                              hex click-location active-id)))))})))

(defmethod event-handler ::text-input
  [{:keys [fx/context ks fx/event]}]
  {:context (fx/swap-context context assoc-in ks event)})

(defmethod event-handler ::change-size
  [{:keys [fx/context direction]}]
  (let [layout (subs/layout context)
        step (if (= direction :plus) 0.1 -0.1)
        scale (-> (+ (:scale layout) step) (max 0.35) (min 2.5))]
    {:context (fx/swap-context context assoc-in [:gui :layout :scale] scale)}))

(defonce last-map-zoom (atom 0))

(defmethod event-handler ::battle-map-scroll
  [{:keys [^ScrollEvent fx/event]}]
  (.consume event)
  (let [now (System/nanoTime)]
    (when (and (not (zero? (.getDeltaY event)))
               (> (- now @last-map-zoom) 150000000))
      (reset! last-map-zoom now)
      {:dispatch {:event-type ::change-size
                  :direction (if (pos? (.getDeltaY event)) :plus :minus)}})))

(defmethod event-handler ::battle-key-pressed
  [{:keys [^KeyEvent fx/event]}]
  (let [code (.getCode event)
        zoom (when (.isControlDown event)
               (cond
                 (#{KeyCode/PLUS KeyCode/ADD KeyCode/EQUALS} code) :plus
                 (#{KeyCode/MINUS KeyCode/SUBTRACT} code) :minus))
        pan (when-not (.isControlDown event)
              (case code
                KeyCode/LEFT [-1 0]
                KeyCode/RIGHT [1 0]
                KeyCode/UP [0 -1]
                KeyCode/DOWN [0 1]
                nil))]
    (cond
      zoom (do (.consume event)
               {:dispatch {:event-type ::change-size :direction zoom}})
      pan (when-let [pane ^ScrollPane (.lookup (.getRoot (.getScene ^Node (.getTarget event))) ".battle-map-scroll")]
            (let [[dx dy] pan
                  bounds (.getLayoutBounds (.getContent pane))
                  viewport (.getViewportBounds pane)
                  width (max 1.0 (- (.getWidth bounds) (.getWidth viewport)))
                  height (max 1.0 (- (.getHeight bounds) (.getHeight viewport)))]
              (.setHvalue pane (-> (+ (.getHvalue pane) (/ (* dx 90.0) width)) (max 0.0) (min 1.0)))
              (.setVvalue pane (-> (+ (.getVvalue pane) (/ (* dy 90.0) height)) (max 0.0) (min 1.0)))
              (.consume event))))))

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
  (let [game (subs/game context)
        gui (subs/gui context)
        selected (cu/select-unit (:units game) unit)
        ;; Until Kevin deployment is implemented, local skirmishes allow
        ;; selecting either force's undeployed units during deployment.
        deployment? (and (= :deployment (:current-phase game))
                         (fx/sub-val context :singleplayer?)
                         selected
                         (= :local-player (:unit-group/player
                                          (some #(when (= (:unit-group/keyword %) (:unit/battle-force selected)) %)
                                                (:forces game))))
                         (not (:unit/acted? selected)))]
    {:context (fx/swap-context context assoc :gui
                               (if deployment?
                                 (assoc gui :active-unit unit :turn-flag false)
                                 (turn-manager/switch-unit game gui unit)))}))

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

(declare advance-kevin-deployment)

(defmethod event-handler ::next-phase
  [{:keys [fx/context]}]
  (let [game (subs/game context)
        advanced (turn-manager/advance-turn game (subs/gui context) (subs/layout context))
        phase-game (or (:game advanced) advanced)
        phase-gui (or (:gui advanced) (subs/gui context))
        auto? (and (fx/sub-val context :singleplayer?)
                   (true? (fx/sub-val context get-in [:lobby :options :smart-auto-deploy]))
                   (= :initiative (:current-phase game))
                   (= :deployment (:current-phase phase-game)))
        result (when auto? (strategic-deployment/auto-deploy-from-initiative game deployment-zone-depth))
        pre-kevin-game (if result (:game result) phase-game)
        kevin (when (and (fx/sub-val context :singleplayer?) (not auto?))
                (advance-kevin-deployment pre-kevin-game true))
        next-game (if kevin (:game kevin) pre-kevin-game)
        problem (cond
                  (and result (seq (:unplaced result)))
                  (str "Could not automatically place " (count (:unplaced result))
                       " units. Complete deployment manually.")
                  (:blocked? kevin) "Kevin could not find a legal deployment hex. Review deployment zones and space.")]
    {:context (fx/swap-context context (fn [state]
                 (-> state
                     (assoc :game next-game :gui phase-gui)
                     (assoc-in [:gui :deployment-error] problem))))
     :dispatch {:event-type ::open-round-dialog}}))

;; Deployment Phase
(defn advance-kevin-deployment [game singleplayer?]
  (if singleplayer?
    (strategic-deployment/auto-deploy-kevin-turns game deployment-zone-depth)
    {:game game :blocked? false}))

(defmethod event-handler ::deploy-unit
  [{:keys [fx/context]}]
  (let [game (subs/game context)
        active-unit (subs/active-unit context)
        location (:unit/location active-unit)
        staged? (and active-unit (movement/deployed? active-unit))
        force (some #(when (= (:unit/battle-force active-unit) (:unit-group/keyword %)) %) (:forces game))
        player-controlled? (and (fx/sub-val context :singleplayer?)
                                (= :deployment (:current-phase game)))
        problem (cond
                  (and player-controlled? (not= :local-player (:unit-group/player force)))
                  "Kevin deploys his own units automatically. Select a unit from your force."
                  (and player-controlled? (not= (:unit/battle-force active-unit) (first (:turn-order game))))
                  "Wait for your force's deployment turn."
                  (not staged?) "Select a unit, then click a highlighted deployment hex."
                  :else (deployment-placement-result game active-unit location))
        conn (subs/connection context)
        player (subs/client-player context)]
    (if problem
      {:context (fx/swap-context context assoc-in [:gui :deployment-error] problem)}
      (let [{:keys [game gui]} (turn-manager/deploy-unit game (subs/gui context))
            kevin (advance-kevin-deployment game (fx/sub-val context :singleplayer?))
            game (:game kevin)
            gui (assoc gui :deployment-error
                       (when (:blocked? kevin)
                         "Kevin could not find a legal deployment hex. Review deployment zones and space."))]
        (when (and conn (not (fx/sub-val context :singleplayer?)))
          (client/deploy-unit-message conn player (:unit/id active-unit)))
        {:context (fx/swap-context context assoc :game game
                                   :gui gui)}))))

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

