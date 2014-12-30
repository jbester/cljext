;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File     : macros.clj
;; Function : macros
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

(ns cljext.macros
  (:refer-clojure)
  (:require [cljext.seq]))

(defmacro switch
  "Switch similar to code e.g. (switch 'a 'b (print 'b) 'c (print 'c) :default (print 'default))"
  ([value & cases]
   (let [sym (gensym)]
     `(let [~sym ~value]
	(cond 
	 ~@(apply concat 
		  (for [case (partition 2 cases)]
		    (if (= (first case) :default)
		      `(true ~(second case))
		      `((= ~sym ~(first case)) ~(second case))))))))))


(defmacro with-syms
  "Generate symbols for a list of variables"
  ([syms & body]
     `(let ~(apply vector
		   (apply concat
			  (for [sym syms]
			    `(~sym (gensym)))))
	~@body)))

(defmacro cl-style-let*
  "(cl-style-let* ((v1 exp1) (v2 exp2)) & body)
Converts a cl-style let* to a clojure-style let"
  ([terms & body]
   `(let [~@(apply concat terms)]
      ~@body)))

(defmacro cl-style-let
  "(cl-style-let ((v1 exp1) (v2 exp2)) & body)
Converts a cl-style let to clojure"
  ([terms & body]
     (let [vars (map first terms)
	   exprs (map rest terms)]
       `((fn [~@vars] ~@body) ~@(for [e exprs] `(do ~@e))))))

(defmacro cl-style-cond 
  "(cl-style-cond (test1 expr1) (test2 exp2) ... )
Converts a cl-style cond to a clojure-style cond"
  ([& conditions]
   `(cond ~@(apply concat conditions))))

(defmacro setq
  "set quoted reference to value expression"
  ([var value]
   `(dosync 
     (ensure ~var)
     (ref-set ~var ~value))))

(defmacro with-default-values
  "Binds a default-value to a variable if one doesn't already exist
typically used when using optional parameters and need a default value
but don't want to use a keyword argument

 (let [a 3] (println a)) => (let [a (if (nil? a) 3 a)] (println a))
"
  ([var-bindings & body]
     (let [var-bindings (partition 2 var-bindings)
	   var-bindings (map (fn [[var binding]]
			       `(~var (if (nil? ~var) ~binding ~var)))
			     var-bindings)]

       `(let [~@(cljext.seq/flatten-1 var-bindings)]
	  ~@body))))
		      
(defmacro def-
  "Define a private variabe"
  ([var & [val]]
     `(def ~(with-meta var (assoc (meta var) :private true)) ~val)))


(defn fn-if
  "Returns a function operating an if, each parameter is a function that 
takes in one parameter"
  ([test then & [else]]
     (fn [param]
       (if (test param)
	 (then param)
	 (if (nil? else)
	   nil
	   (else param))))))

(defmacro thunk
  "Construct a thunk (a lambda with no parameters)"
  ([ & body ] 
     `(fn [] ~@body)))

(defmacro n-thunk
  "Construct a thunk (a lambda with parameters that are ignroed)"
  ([ & body ] 
     (let [param (gensym)]
	   `(fn [& ~param] ~@body))))
	

(defmacro aif 
  "Anaphoric If - binds the condition to it "
  ([pred then & [else]] 
     (let [it (symbol "it")]
       `(let [~it ~pred]
	  (if ~it
	    ~then
	    ~else)))))

(defmacro awhen 
  "Anaphoric When - binds the condition to it "
  ([pred then] 
     (let [it (symbol "it")]
       `(let [~it ~pred]
	  (when ~it
	    ~then)))))

(defmacro aunless 
  "Anaphoric unless - binds the condition to it "
  ([pred then] 
     (let [it (symbol "it")]
       `(let [~it ~pred]
	  (unless ~it
		  ~then)))))

(defmacro acond 
  "Anaphoric condition"
  ([& conds] 
   (let [it (symbol "it")
	 value (gensym)]
     ;; expands 
     `(with-local-vars [~value nil]
	(cl-style-cond ~@(for [term (partition 2 conds)] 
			   `((var-set ~value ~(first term)) 
			     (let [~it @~value]
			       ~@(rest term))))
			   )))))
