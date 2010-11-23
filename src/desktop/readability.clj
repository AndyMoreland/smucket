(ns desktop.readability
  (:require [clojure.contrib.string :as st]
            [infer.measures :only [sparse-dot-product] :as im]
            [clojure.contrib.profile :as p]
            [desktop.htmlparser :as parser])
  (:import [org.apache.commons.lang StringEscapeUtils]))

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
  :child-paragraphs
  (count (parser/children-matching node #(= (.getNodeName %) "p")))
   
  :bad-id
  (count (re-seq bad-words (parser/attribute node "id")))
  
  :bad-class
  (count (re-seq bad-words (parser/attribute  node "class")))
  
  :good-id
  (count (re-seq good-words (parser/attribute node "id")))
  
  :good-class
  (count (re-seq good-words (parser/attribute node "class")))
  
  :inner-divs
  (count (parser/children-matching node #(= (.getNodeName %) "div")))
  )

(defn clean-content-divs [divs]
  (doseq [div divs]
    (parser/remove-children-matching div
                              #(not (= 0
                                       (+ (count (re-seq bad-words (parser/attribute % "class")))
                                          (count (re-seq bad-words (parser/attribute % "id"))))))))
  (identity divs))

(defn clean-content [^String content]
  (st/lower-case (st/replace-re punctuation " " (st/replace-re #"&apos;" "'" (st/replace-re #"(\n|\r)+" " " (StringEscapeUtils/unescapeHtml content))))))

(defn find-content-div [dom]
  "Returns the node that most likely to contain the majority of the content of the page"
  (let [clean-divs (filter #(not (nil? %)) (parser/elements parser/dom "div"))]
    (if (not (= 0 (count clean-divs)))
      (let [content-div (apply max-key
                               (fn [div] (infer.measures/sparse-dot-product (weight-div div) div-weight-map))
                               clean-divs)]
        (if (> (count (.getTextContent content-div)) 50) content-div nil)) nil)))

(defn find-content [^String url]
  (let [content-div (find-content-div (parser/dom url))]
    (if content-div (clean-content (.getTextContent content-div)) nil)))

(defn words [^String string]
  (st/split #" " string))
