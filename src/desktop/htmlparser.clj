(ns desktop.htmlparser.core
  (:require [clojure.contrib.string :as st]
            [infer.measures :only [sparse-dot-product] :as im]
            [clojure.contrib.profile :as p])
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
     #"comment|comments|com|reviews|review|footer|meta|footnote|foot|navigation|side|sidebar|nav|container|module|answer")

(def good-words
     ; Classes/IDs matching these are "good"
     #"article|post|hentry|entry|content|text|body|article|main|post-text|entry-text")
(def punctuation
     #"[!\"#$%&\'()*+,-./:;<=>?@\[\][\\]^_`{|}~]")

(def div-weight-map {
                     :child-paragraphs 150
                     :bad-id -150
                     :bad-class -150
                     :good-id 75
                     :good-class 75
                     :num-words 0.01
                     :commas 1
                     :inner-divs -50
                     :long-text? 100.0 } )

(defn weight-div [node]
  "Takes an HTML node and returns a map containing numeric 'facts' about the div that are important; will be cross-producted with the weight map above"
  {:child-paragraphs
   (p/prof :child-paragraphs (count (children-matching node #(= (.getNodeName %) "p"))))
   
   :bad-id
   (p/prof :bad-id (count (re-seq bad-words (.getAttribute node "id"))))
   
   :bad-class
   (p/prof :bad-class (count (re-seq bad-words (.getAttribute node "class"))))
   
   :good-id
   (p/prof :good-id (count (re-seq good-words (.getAttribute node "id"))))
   
   :good-class
   (p/prof :good-class (count (re-seq good-words (.getAttribute node "class"))))
   
   :num-words
   (p/prof :num-words (count-words (.getTextContent node)))
   
   :commas
   (p/prof :commas (count (re-seq #"," (.getTextContent node))))

   :general-punctuation
   (p/prof :general-punctuation (count (re-seq punctuation (.getTextContent node))))
   
   :inner-divs
   (p/prof :inner-divs (count (children-matching node #(= (.getNodeName %) "div"))))
   
   :long-text?
   (p/prof :long-text? (if (> (count (.getTextContent node)) 30) 1.0 0.0))})

(defn initialize-cleaner-props [cleaner]
  "Takes an HTMLCleaner object and makes it ignore comments, script tags, and style tags"
  (doto (.getProperties cleaner)
    (.setOmitComments true)
    (.setPruneTags "script,style")))

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


(defn clean-content-divs [divs]
  (doseq [div divs]
    (remove-children-matching div
                              #(not (= 0
                                       (+ (count (re-seq bad-words (attribute % "class")))
                                          (count (re-seq bad-words (attribute % "id")))))))))

(defn find-content-div [dom]
  "Returns the node that most likely to contain the majority of the content of the page"
  (let [divs (elements dom "div")]
    (if (not (= 0 (count divs)))
      (let [content-div (apply max-key
                               (fn [div] (infer.measures/sparse-dot-product (weight-div div) div-weight-map))
                               divs)]
        (if (> (count (.getTextContent content-div)) 50) content-div nil)) nil)))



(defn clean-content [^String content]
  (st/lower-case (st/replace-re punctuation " " (st/replace-re #"&apos;" "'" (st/replace-re #"(\n|\r)+" " " (StringEscapeUtils/unescapeHtml content))))))

(defn find-content [^String url]
  (let [content-div (p/prof :find-content (find-content-div (dom url)))]
    (if content-div (clean-content (.getTextContent content-div)) nil)))

(defn words [^String string]
  (st/split #" " string))

