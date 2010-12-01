(ns smucket.core
  (:gen-class)
  (:require [clojure.contrib.string :as st]
            [somnium.congomongo :as cm]
            [desktop webloc])
  (:import [org.joda.time DateTime Instant]
           java.io.File
           java.util.Date
           java.lang.Thread))


(def desktop-dir "/Users/andrew/Desktop")
(def keep-running (atom true))
(def bad-files #{".DS_Store" ".localized"})
(def callbacks {"webloc" desktop.webloc/process})

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
  (println (desktop.webloc/process file)))

(defn process
  "run callbacks on a changed file"
  [file]
  ;;; check filetype, check name, run registered callbacks
  (callback file))

(defn callback [file date]
  (println (str "Processing: " file ))
  (let [file-extensions (extract-file-extensions file)]
    (cm/insert! :files
                (into {:file file :date date}
                              (reduce
                               (fn [acc [ext f]] (conj acc {ext (f file)})) {}
                               (filter #(some #{(first %)} file-extensions) callbacks))))))

(defn run []
  (loop [date (Date.)]
    (Thread/sleep 1000)
    (println "Firing.")
    (doseq [file (filter #(>= 0 (.compareTo date (Date. (last-modified %)))) (list-desktop-files))]
      (future (process file date)))
    
    (if @keep-running
      (recur (Date.))
      (println "Terminating"))))

(defn -main [& args]
  "Starts the loop in another thread"
  (cm/mongo! :db "desktop")
 
  (def keep-running (atom true))
  (.start (Thread. run)))

(defn stop []
  "Stops the loop"
  (reset! keep-running false))

