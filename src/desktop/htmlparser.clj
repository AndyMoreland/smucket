(ns desktop.htmlparser
  (:require [clojure.contrib.string :as st]
            [infer.measures :only [sparse-dot-product] :as im]
            [clojure.contrib.profile :as p])
  (:import [org.htmlcleaner HtmlCleaner DomSerializer]
           [org.apache.commons.lang StringEscapeUtils]
           [org.w3c.dom Document Node]))

(defn initialize-cleaner-props [cleaner]
  "Takes an HTMLCleaner object and makes it ignore comments, script tags, and style tags"
  (doto (.getProperties cleaner)
    (.setOmitComments true)
    (.setPruneTags "script,style")))

(defn do-children [node func]
  "Analagous to running #map over a collection of children"
  (if (not (and node (.hasChildNodes node)))
    []
    (let [children (.getChildNodes node)]
      (doall (for [i (range 0 (.getLength children))]
               (func (.item children i)))))))

(defn children-matching [node pred]
  "Returns children matching a predicate"
  (filter pred (do-children node identity)))

(defn elements [^Document dom ^String tag]
  "Returns a collection of elements matching the given tag name in the dom"
  (let [nodes (.getElementsByTagName dom tag)]
    (map identity
         (for [i (range 0 (.getLength nodes))]
           (.item nodes i)))))


(defn remove-children-matching [^Node node pred]
  "Removes children matching a predicate and returns the node"
  (doseq [child (children-matching node pred)] (.removeChild node child))
  node)

(defn attribute [^Node node ^String name]
  (let [attributes (.getAttributes node)
        attribute (if attributes (.getNamedItem attributes name) nil)]
    (if attribute (.getValue attribute) "")))

(defn dom [url]
  "Returns cleaned html source given url"
  (let [
        cleaner (HtmlCleaner.)
        props (initialize-cleaner-props cleaner)
        node (.clean cleaner (slurp url))
        serializer (DomSerializer. props)
        ]
    (.createDOM serializer node)))

;;;;;; These are the two functions that should really be called




