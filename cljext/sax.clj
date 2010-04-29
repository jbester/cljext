;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File     : sax.clj
;; Function : XML SAX library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Send comments or questions to code at freshlime dot org
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (c) 2008, J. Bester
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ``AS IS'' AND ANY
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;   Example:
;;   prints the element names as encounted by the parser
;;
;;   (defn start [uri localname & rest]
;;     (println localname))
;;
;;   (cljext.sax/with-xml-handler 
;;    [:start-element start] (cljext.sax/parse-xml "/path/to/file.xml"))


(ns cljext.sax
    (:refer-clojure)
    (:import [org.xml.sax XMLReader]
	     [org.xml.sax.helpers XMLReaderFactory DefaultHandler])
    )


;; binding for the handler to parse with
(def *xml-handler*)

;; set of all valid events
(def +valid-events+
     (set
      [:fatal-error :errors :warning
      :start-document :end-document :start-element :end-element
      :characters
      :start-prefix-mapping :end-prefix-mapping
      :ignorable-whitespace :notation-decl :processing-instruction :resolve-entity 
      :skipped-entity :unparsed-entity-decl]))

;; handler structure used by proxy handler
(defstruct handler  
  :fatal-error :errors :warning
  :start-document :end-document :start-element :end-element
  :characters
  :start-prefix-mapping :end-prefix-mapping
  :ignorable-whitespace :notation-decl :processing-instruction :resolve-entity 
  :skipped-entity :unparsed-entity-decl )     

;; used by make-proxy-handler
;; in effect we proxy the default handler and call methods
;; defined in our handler structure if they exist
(defmacro shadow-method
  ([keyword params]
   (let [method (gensym)]
   `(let [~method (~keyword *xml-handler*)]
      (when ~method
	     (~method ~@params))))))

;; create a new proxy handler calls our handler structure
;; as bound by xml-handler upon event
(defn- make-proxy-handler
  ([]
   (proxy [org.xml.sax.helpers.DefaultHandler]
	  []

	  (characters 
	   [ch start len]
	   (shadow-method :characters [ch start len]))
	  
	  (endDocument 
	   []
	   (shadow-method :end-document []))

	  (endElement 
	   [uri localName qName]
	   (shadow-method :end-element [uri localName qName]))

	  (endPrefixMapping 
	   [prefix]
	   (shadow-method :end-prefix-mapping [prefix]))

	  (error 
	   [e]
	   (shadow-method :error [e]))

	  (fatalError
	   [e]
	   (shadow-method :fatal-error [e]))

	  (ignorableWhitespace
	   [ch start len]
	   (shadow-method :ignorable-whitespace [ch start len]))

	  (notationDecl
	   [name publicId systemId]
	   (shadow-method :notation-decl [name publicId systemId]))

 	  (processingInstruction
	   [s data]
	   (shadow-method :processing-instruction [s data]))

 	  (resolveEntity
	   [s]
	   (shadow-method :resolve-entity [s]))

 	  (skippedEntity
	   [name]
	   (shadow-method :skipped-entity [name]))

 	  (startDocument
	   []
	   (shadow-method :start-document []))

 	  (startElement 
	   [uri localName qName attrs]
	   (shadow-method :start-element [uri localName qName attrs]))

 	  (startPrefixMapping
	   [prefix uri]
	   (shadow-method :start-prefix-mapping [prefix uri]))

 	  (warning
	   [e]
	   (shadow-method :warning [e]))


   )))

  
;; create a reader with a new proxy handler for each reader
(defn- make-reader
  ([] 
   (let [reader (XMLReaderFactory/createXMLReader)
	 handler (make-proxy-handler)]
     (do
       (.setContentHandler reader handler)
       (.setErrorHandler reader handler)
       reader))))

(defn make-xml-handler
  "(make-xml-handler methods)
Create a new xml handler.  Validates all events are valid
prior to creating.

methods - a vaid event followed by a function
"
  ([& methods]
   (do
     ;; check parser arguments
     (let [methods (partition 2 methods)]
       (doseq [entry methods]
	 (let [event (first entry)]
 	   (when-not (contains? +valid-events+ event)
	     (throw
	      (new IllegalArgumentException 
		   #^String (format "%s is not a valid sax parser event"
				    event)))))))
     
     (apply struct-map (cons handler methods)))))


;; if xml-handler is stored
;; allow multiple parsers to use the same handler
(defmacro with-existing-xml-handler
  "(with-existing-xml-handler handler & body)

Execute with appropriate binding to *xml-handler*

handler - an xml handler to use
body - expressions to execute

"
  ([handler & body]
   `(binding [*xml-handler* ~handler]
      ~@body)))

(defmacro with-xml-handler
  "(with-xml-handler [params] & body)

Create a new handler and execute a body

params - vector of parameters for make-handler
body - statements to execute

"
  ([params & body]
   `(binding [*xml-handler* (make-xml-handler ~@params)]
      ~@body)))



(defn parse-xml
  "(parse-xml xml)
Parse an xml file

xml - a string or an input stream

Returns: 
nil

 (parse-xml handler xml)
Parse an xml file with a given handler (overrides *xml-handler*)

handler - handler as defined by make-xml-handler
xml - a string or an input straem

Returns nil
"
  ([xml]
   (let [reader (make-reader)]
     (.parse reader xml)))

  ([handler xml]
   (binding [*xml-handler* handler]
     (let [reader (make-reader)]
       (.parse reader xml)))))


