(ns megastrike.gui.theme.theme
  (:require [clojure.java.io :as io]))

(def stylesheet
  (-> "styles/theme.css" io/resource .toExternalForm))
