(ns desktop.webloc
  (:require [clojure.contrib.string :as st]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.contrib.zip-filter.xml :as zf]
            [desktop.htmlparser :as p]
            [desktop.readability :as r])
  (:import java.io.File
           java.util.Date))


(defn extract-url-from-webloc [file]
  (first (zf/xml-> (zip/xml-zip (xml/parse (File. file))) :dict :string zf/text)))


(defn count-words [string]
  (count (st/split #" " string)))

(defn extract-title [url]
  (let [title-element (first (p/elements (p/dom url) "title"))]
    (when title-element (.getTextContent title-element)))
  )

(defn process [file]
  (let [url (extract-url-from-webloc file)]
    (when (= 0 (count (re-seq #"(jpg|png|gif|bmp)" url)))
      (let [title (extract-title url)
            content (r/find-content url)
            content-word-frequencies-map (when content (frequencies (r/words content)))]
        {:url url, :title title, :content content, :content-word-frequencies content-word-frequencies-map}))))
