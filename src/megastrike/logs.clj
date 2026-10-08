(ns megastrike.logs
  (:require
   [clojure.java.io :as io]
   [com.brunobonacci.mulog :as mu]
   [megastrike.utils :as utils]))

(def log-file (str utils/application-directory "megastrike.log"))

(def logs
  (if (Boolean/getBoolean "megastrike.test")
    (fn [])
    (do
      (io/delete-file log-file true)
      (mu/start-publisher! {:type :multi
                           :publishers
                           [{:type :console :pretty? true}
                            {:type :simple-file :filename log-file}]}))))
