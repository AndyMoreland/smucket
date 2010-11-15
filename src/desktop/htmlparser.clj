(ns desktop.htmlparser.core
  (:require [clojure.contrib.string :as st]
            [infer.measures :only [sparse-dot-product]] )
  (:import [org.htmlcleaner HtmlCleaner DomSerializer]
           [org.apache.commons.lang StringEscapeUtils]
           [org.w3c.dom Document Node]))

                                        ; TODO: make sure there are spaces after punctuation
                                        ; TODO: Replace &apos; correctly


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

(defn count-words [string]
  (count (st/split #" " string)))


(def bad-words
     ; Classes/IDs matching these are "bad"
     #"comment|comments|com|reviews|review|footer|meta|footnote|foot|navigation")

(def good-words
     ; Classes/IDs matching these are "good"
     #"article|post|hentry|entry|content|text|body|article|main")

(def div-weight-map {
                     :child-paragraphs 100
                     :bad-id -50
                     :bad-class -50
                     :good-id 25
                     :good-class 25
                     :num-words 0.01
                     :commas 1
                     :inner-divs -1
                     :long-text? 1.0 } )

(defn weight-div [node]
  "Takes an HTML node and returns a map containing numeric 'facts' about the div that are important; will be cross-producted with the weight map above"
  {:child-paragraphs
   (count (children-matching node #(= (.getNodeName %) "p")))
   
   :bad-id
   (count (re-seq bad-words (.getAttribute node "id")))
   
   :bad-class
   (count (re-seq bad-words (.getAttribute node "class")))
   
   :good-id
   (count (re-seq good-words (.getAttribute node "id")))
   
   :good-class
   (count (re-seq good-words (.getAttribute node "class")))
   
   :num-words
   (count-words (.getTextContent node))
   
   :commas
   (count (re-seq #"," (.getTextContent node)))
   
   :inner-divs
   (count (children-matching node #(= (.getNodeName %) "div")))
   
   :long-text?
   (if (> (count (.getTextContent node)) 30) 1.0 0.0)})

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


(defn remove-children-matching [node pred]
  "Removes children matching a predicate and returns the node"
  (doseq [child (children-matching node pred)] (.removeChild node child))
  node)

(defn attribute [node name]
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


(defn find-content-div [dom]
  "Returns the node that most likely to contain the majority of the content of the page"
  (let [content (apply
               max-key
               (fn [div] (infer.measures/sparse-dot-product (weight-div div) div-weight-map))
               (elements dom "div"))]
    (if (> (count (.getTextContent content)) 50) content nil)))

(defn clean-content-div [div]
  (remove-children-matching div
                            #(not (= 0
                                     (+ (count (re-seq bad-words (attribute % "class")))
                                        (count (re-seq bad-words (attribute % "id"))))))))

(defn find-content [url]
  (println "hi")
  (st/replace-re #"(\n|\r)+" " " (StringEscapeUtils/unescapeHtml (.getTextContent (find-content-div (dom url))))))
