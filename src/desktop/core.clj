(ns desktop.core
  (:use somnium.congomongo)
  (:require [clojure.contrib.string :as st])
  (:import [org.joda.time DateTime Instant]
           java.io.File
           java.util.Date))


(def desktop-dir "/Users/andrew/Desktop")

(def last-check (Date.))

(def bad-files #{".DS_Store" ".localized"})

(defn good-file? [file]
  (not (bad-files file))
  )

(defn strip-tag [string tag]
  (st/replace-re (re-pattern (str "(?s)</?" tag ">")) "" string)
  )

(defn isolate-tag [string tag]
  "keep only the contents of a tag"
  (map #(nth % 1) (re-seq (re-pattern (str "(?s)<" tag ">(.*?)</" tag ">")) string))
  )

(defn last-modified
  "returns the last modified time of a given file"
  [file-path]
  (let [file (File. file-path)]
    (.lastModified file)
    )
  )

(defn list-desktop-files
  "returns a list of files on the desktop"
  []
  (let [files (.list (File. desktop-dir))]
    (map #(str desktop-dir "/" %)
         (filter good-file? (seq files)))
    ))

(defn callback [file]
  (insert! :files { :name file
                   :words (frequencies (st/split #"\W+" (slurp file)))})
  )


(defn process
  "run callbacks on a changed file"
  [file]
  ;;; check filetype, check name, run registered callbacks
  ;(callback file)
  )

(defn callback [file]
  (println (frequencies (st/split #"\W+" (slurp file))))
  )

(defn run
  "main loop"
  []
  (doseq [file (filter #(>= 0 (.compareTo last-check (Date. (last-modified %)))) (list-desktop-files))]
    (process file))
  (def last-check (Date.)))  
