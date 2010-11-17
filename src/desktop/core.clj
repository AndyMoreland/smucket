(ns desktop.core
  (:require [clojure.contrib.string :as st]
            [somnium.congomongo :as cm])
  (:import [org.joda.time DateTime Instant]
           java.io.File
           java.util.Date
           java.lang.Thread))


(def desktop-dir "/Users/andrew/Desktop")
(def keep-running (atom true))
(def bad-files #{".DS_Store" ".localized"})

(defn extract-file-extensions [file]
  (rest (st/split #"\." (last (st/split #"\/" file)))))

(defn good-file? [file]
  (not (bad-files file)))

(defn last-modified
  "returns the last modified time of a given file"
  [file-path]
  (let [file (File. file-path)]
    (.lastModified file)))

(defn list-desktop-files
  "returns a list of files on the desktop"
  []
  (let [files (.list (File. desktop-dir))]
    (map #(str desktop-dir "/" %)
         (filter good-file? (seq files)))))

(defn callback [file]
  (cm/insert! :files { :name file
                   :words (frequencies (st/split #"\W+" (slurp file)))}))

(defn process
  "run callbacks on a changed file"
  [file]
  ;;; check filetype, check name, run registered callbacks
  (callback file))

(defn callback [file]
  (println (desktop.htmlparser.core/find-content (desktop.webloc.core/extract-url-from-webloc file)))
  )

(defn run []
  (loop [date (Date.)]
    (Thread/sleep 1000)
    (println "Firing.")
    (doseq [file (filter #(>= 0 (.compareTo date (Date. (last-modified %)))) (list-desktop-files))]
      (process file))
    
    (if @keep-running
      (recur (Date.))
      (println "Terminating"))))

(defn main []
  "Starts the loop in another threaD"
  (def keep-running (atom true))
  (.start (Thread. run)))

(defn stop []
  "Stops the loop"
  (reset! keep-running false))

