;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File     : bst.clj
;; Function : Binary Search Tree Implementation
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
;;     * The name of the authors names of its contributors may be used to 
;;       endorse or promote products derived from this software without
;;       specific prior written permission.
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

(ns cljext.algorithms.bst
  (:refer-clojure)
  (:require [cljext.macros])
  (:refer cljext.macros))

(defstruct bst-node :left :right :key :value)

(defn make-bst 
  ([]
     nil))

(defn make-bst-node 
  ([key value & [left right]]
     (vector key value left right)))

(defn bst-remove
  ([tree key]
     (if (empty? tree)
       nil
       (let [[cur-key cur-value cur-left cur-right :as cur] tree]
	 (cond (> cur-key key)
	       (make-bst-node cur-key cur-value (bst-remove cur-left key) cur-right)
	       (< cur-key key)
	       (make-bst-node cur-key cur-value cur-left (bst-remove cur-right key))
	       true
	       (cond (empty? cur-left)
		     cur-right
		     (empty? cur-right)
		     cur-left
		     true
		     nil))))))

     

(defn insert-node
  ([tree [key :as node]]
     (if (empty? tree)
       node
       (let [[cur-key cur-value cur-left cur-right :as cur] tree]
	 (cond (> cur-key key)
	       (make-bst-node cur-key cur-value (if (nil? cur-left) node (insert-node cur-left node)) cur-right)
	       (< cur-key key)
	       (make-bst-node cur-key cur-value cur-left (if (nil? cur-right) node (insert-node cur-right node)))
	       true
	       cur)))))

(defn bst-insert
  ([tree key value]
     (insert-node tree (make-bst-node key value)))
  ([tree key]
     (insert-node tree (make-bst-node key nil))))
		     
	       

