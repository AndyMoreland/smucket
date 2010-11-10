(ns desktop.htmlparser.core
  (:use [clojure.xml :as xml]
        [clojure.zip :as zip]
        [clojure.contrib.zip-filter.xml :as zf] )
  (:require [clojure.contrib.string :as st])
  (:import [org.htmlcleaner HtmlCleaner DomSerializer]
           [org.apache.commons.lang StringEscapeUtils]
           [org.w3c.dom Document Node]))


(defn initialize-cleaner-props [cleaner]
  "Takes an HTMLCleaner object and makes it ignore comments, script tags, and style tags"
  (doto (.getProperties cleaner)
    (.setOmitComments true)
    (.setPruneTags "script,style")))

(defn elements [dom tag]
  "Returns a collection of elements matching the given tag name in the dom"
  (let [nodes (.getElementsByTagName dom tag)]
    (map identity
         (for [i (range 0 (.getLength nodes))]
           (.item nodes i)))))

(defn do-children [node func]
  "Analagous to running #map over a collection of children"
  (if (not (and node (.hasChildNodes node)))
    []
    (let [children (.getChildNodes node)]
      (doall (for [i (range 0 (.getLength children))]
               (func (.item children i)))))))

(defn children-matching [node pred]
  "Returns children matching a predicate"
  (filter pred (do-children node identity))
  )

(defn most-child-paragraphs [nodes]
  "Returns the node with the largest number of child paragraphs"
  (first (reverse
          (sort-by (fn [node] (count (children-matching node #(= (.getNodeName %) "p"))))
                   nodes))))

(defn find-content-div [dom]
  "Returns the node that most likely to contain the majority of the content of the page"
  
  )

(defn dom [url]
  "Returns cleaned html source given url"
  (let [
        cleaner (HtmlCleaner.)
        props (initialize-cleaner-props cleaner)
        node (.clean cleaner (slurp url))
        serializer (DomSerializer. props)
        ]
    (.createDOM serializer node)))

(defn count-words [string]
  (count (st/split #" " string)))

