(ns desktop.webloc.core
  (:use somnium.congomongo
        [clojure.xml :as xml]
        [clojure.zip :as zip]
        [clojure.contrib.zip-filter.xml :as zf] )
  (:require [clojure.contrib.string :as st]
            [net.cgrand.enlive-html :as html])
  (:import java.io.File
           java.util.Date
           [org.htmlcleaner HtmlCleaner SimpleXmlSerializer]
           [org.apache.commons.lang StringEscapeUtils]))


(defn initialize-cleaner-props [cleaner]
  "Takes an HTMLCleaner object and makes it ignore comments, script tags, and style tags"
  (doto (.getProperties cleaner)
    (.setOmitComments true)
    (.setPruneTags "script,style"))
  )


(defn isolate-tag [source, tag]
  (str (.getText (.findElementByName
                  (let [cleaner (HtmlCleaner.)]
                    (initialize-cleaner-props cleaner)
                    (.clean cleaner source)) tag true))))

(defn extract-url-from-webloc [file]
  (first (zf/xml-> (zip/xml-zip (xml/parse file)) :dict :string zf/text))
  )

(defn source-for [url]
  "Returns cleaned html source given url"
  (let [
        cleaner (HtmlCleaner.)
        props (initialize-cleaner-props cleaner)
        node (.clean cleaner (slurp "http://www.google.com"))
        serializer (SimpleXmlSerializer. props)
        ]
    (.getXmlAsString serializer node))
  )

(defn scrape [url]
  (html/html-resource (java.net.URL. url)))

(defn count-words [string]
  (count (st/split #" " string)))

(defn string-to-xml-struct-map [string]
  (xml/parse (java.io.ByteArrayInputStream. (.getBytes string))))

(defn process-webloc [file]
  (let [url (extract-url-from-webloc file)]
    (isolate-tag (slurp url) "body")
    )
  )
