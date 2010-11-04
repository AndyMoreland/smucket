(ns desktop.core
  (:use (clj-time core))
  (:import (java.io File))
  (:import (org.joda.time DateTime Instant)))

(def desktop-dir "/Users/andrew/Desktop")

(def last-check (now))

(defn good-file? [file]
  (and (not (= ".DS_Store" file)) (not (= ".localized" file)))
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

(defn process
  "run callbacks on a changed file"
  [file]
  ;;; check filetype, check name, run registered callbacks
  (callback file)
  )

(defn callback [file]
  (println (frequencies (split #"\W+" (slurp file))))
  )

(defn run
  "main loop"
  []
  (doseq [file (filter #(>= 0 (.compareTo last-check (Date. (last-modified %)))) (list-desktop-files))] (process file))
  (def last-check (Date.)))
  



