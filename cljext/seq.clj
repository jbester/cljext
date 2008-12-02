;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File     : seq.clj
;; Function : sequence library
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

(ns cljext.seq
  (:refer-clojure)
  )


(defn map*
  "Map that allows mismatch sized lists 
e.g. (map* vector '(1 2) '(3 4 5)) => ([1 3] [2 4] [nil 5])
"
  ([fn & cols]
     (let [len (map count cols)]
       (loop [result nil
	      cols cols]
	 (if (every? empty? cols)
	   (reverse result)
	   (recur (conj result (apply fn (map first cols)))
		  (map rest cols)))))))

(defn zip 
  "Zip two or more lists together"   
  ([& cols] 
     (apply map vector cols)))

(defn zip*
  "Zip two or more lists together allows uneven lists"   
  ([& cols]
     (apply map* vector cols)))

(defn lazy-zip
  "A lazy version of zip"
  ([& cols]
     (when (every? not-empty cols)
       (lazy-cons (map first cols) (apply lazy-zip (map rest cols))))))

(defn lazy-zip*
  "A lazy version of zip*"
  ([& cols]
     (when (some not-empty cols)
       (lazy-cons (map first cols) (apply lazy-zip* (map rest cols))))))

(defn unzip 
  "Unzip one list into a list of lists"
  ([col] 
     (let [length (count col)]
       (partition length (apply interleave col)))))

(defn enumerate 
  "Enumerate a list e.g. '(a b c d) => '([0 a] [1 b] [2 c] [3 d])"
  ([col] 
     (zip (iterate inc 0) col)))

(defn count-if
  "Count number of times predicate true in a list"
  ([pred col]
     (loop [count 0
	    col col]
       (if (empty? col)
	 count
	 (recur 
	  (if (pred (first col)) (inc count) count) 
	  (rest col))))))

(defn count-occurances
  "Count number of occurances in a list of specified item"
  ([item col]
     (count-if (fn [i] (= i item)) col)))

(defn member? 
  "Test for membership in a list"
  ([el col pred]
   (loop [col col]
     (let [[hd & tl] col]
       (cond (empty? col)
	     false
	     (pred el hd)
	     hd
	     true
	     (recur tl)))))
  ([el col]
   (member? el col =)))


(defn positions
  "Return the list of positions where a given element resides"
  ([el col]
     (loop [col (enumerate col)
	    result nil]
       (let [[idx elem] (first col)]
	 (if (empty? col)
	   (reverse result)
	   (recur (rest col)
		  (if (= elem el) (cons idx result) result)))))))
	   
       
(defn list-tabulate
  "Create a list by calling the provided func with the position as a parameter"
  ([n func]
     (for [i (range n)]
       (func i))))