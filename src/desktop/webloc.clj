(ns desktop.webloc.core
  (:require [clojure.contrib.string :as st]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.contrib.zip-filter.xml :as zf] )
  (:import java.io.File
           java.util.Date
           [org.htmlcleaner HtmlCleaner DomSerializer]
           [org.apache.commons.lang StringEscapeUtils]
           [org.w3c.dom Document Node]))


(defn extract-url-from-webloc [file]
  (first (zf/xml-> (zip/xml-zip (xml/parse file)) :dict :string zf/text)))

(defn count-words [string]
  (count (st/split #" " string)))
