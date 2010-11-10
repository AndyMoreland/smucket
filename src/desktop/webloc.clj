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


(defn initialize-cleaner-props [cleaner]
  "Takes an HTMLCleaner object and makes it ignore comments, script tags, and style tags"
  (doto (.getProperties cleaner)
    (.setOmitComments true)
    (.setPruneTags "script,style"))
  )


(defn extract-url-from-webloc [file]
  (first (zf/xml-> (zip/xml-zip (xml/parse file)) :dict :string zf/text))
  )

(defn dom [url]
  "Returns cleaned html source given url"
  (let [
        cleaner (HtmlCleaner.)
        props (initialize-cleaner-props cleaner)
        node (.clean cleaner (slurp url))
        serializer (DomSerializer. props)
        ]
    (.createDOM serializer node))
  )

(defn count-words [string]
  (count (st/split #" " string)))

(defn process-webloc [file]
  (let [url (extract-url-from-webloc file)]
    (isolate-tag (slurp url) "body")
    )
  )
