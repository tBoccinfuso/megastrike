(ns megastrike.strategic-deployment-test
  (:require [clojure.test :refer [deftest is]]
            [megastrike.board :as board]
            [megastrike.deployment :as deployment]
            [megastrike.strategic-deployment :as strategic]))

(defn fixture [size]
  {:current-phase :initiative
   :turn-number 1
   :round-report ""
   :game-board (board/create-board size size)
   :forces [{:unit-group/keyword :blue :unit-group/deployment :direction/n :initiative 4}
            {:unit-group/keyword :red :unit-group/deployment :direction/s :initiative 8}]
   :units [{:unit/id "b1" :unit/battle-force :blue :unit/location {}
            :unit/attacks {:attack/regular {:attack/s 2 :attack/m 1 :attack/l 0}}
            :unit/move-modes {:move/walk 5}}
           {:unit/id "r1" :unit/battle-force :red :unit/location {}
            :unit/attacks {:attack/regular {:attack/s 1 :attack/m 2 :attack/l 3}}
            :unit/move-modes {:move/walk 4}}]})

(deftest auto-deploy-advances-to-movement
  (let [game (fixture 8)
        result (strategic/auto-deploy-from-initiative game 2)]
    (is (:advanced? result))
    (is (= :movement (get-in result [:game :current-phase])))
    (is (empty? (:unplaced result)))
    (is (every? deployment/deployed? (get-in result [:game :units])))
    (is (= :initiative (:current-phase game)))))

(deftest no-phase-skip-on-failed-placement
  (let [result (strategic/auto-deploy-from-initiative (fixture 1) 1)]
    (is (not (:advanced? result)))
    (is (= :deployment (get-in result [:game :current-phase])))
    (is (seq (:unplaced result)))))

(deftest non-initiative-is-unchanged
  (let [game (assoc (fixture 8) :current-phase :movement)
        result (strategic/auto-deploy-from-initiative game 2)]
    (is (= game (:game result)))
    (is (false? (:advanced? result)))))

(deftest kevin-deploys-on-his-own-turn-only
  (let [game (-> (fixture 8)
                 (assoc :current-phase :deployment :turn-order [:red :blue])
                 (assoc-in [:forces 1 :unit-group/player] :kevin)
                 (assoc-in [:forces 0 :unit-group/player] :local-player))
        result (strategic/auto-deploy-kevin-turns game 2)]
    (is (false? (:blocked? result)))
    (is (= [:blue] (vec (get-in result [:game :turn-order]))))
    (is (deployment/deployed? (second (get-in result [:game :units]))))
    (is (not (deployment/deployed? (first (get-in result [:game :units])))))
    (is (true? (:unit/acted? (second (get-in result [:game :units])))))))

(deftest kevin-waits-for-human-deployment
  (let [game (-> (fixture 8)
                 (assoc :current-phase :deployment :turn-order [:blue :red])
                 (assoc-in [:forces 1 :unit-group/player] :kevin)
                 (assoc-in [:forces 0 :unit-group/player] :local-player))
        result (strategic/auto-deploy-kevin-turns game 2)]
    (is (= game (:game result)))
    (is (empty? (:placements result)))))

(deftest kevin-fails-without-consuming-turn
  (let [game (-> (fixture 1)
                 (assoc :current-phase :deployment :turn-order [:red :blue])
                 (assoc-in [:forces 1 :unit-group/player] :kevin)
                 (assoc-in [:units 0 :unit/location] (deployment/location-key
                                                     (first (:tiles (:game-board (fixture 1)))))))
        result (strategic/auto-deploy-kevin-turns game 1)]
    (is (:blocked? result))
    (is (= [:red :blue] (:turn-order (:game result))))
    (is (not (deployment/deployed? (second (:units (:game result))))))))
