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


(defn vector-map*
  "Vector Map that allows mismatch sized inputs
e.g. (vector-map* vector '(1 2) '(3 4 5)) => ([1 3] [2 4] [nil 5])
"
  ([fn & cols]
     (let [len (map count cols)]
       (loop [result []
	      cols cols]
	 (if (every? empty? cols)
	   result
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
     (lazy-seq
      (when-let [s (seq cols)]
        (cons (apply vector (map first s)) (apply map vector (map rest s)))))))

(defn lazy-zip*
  "Lazily Zip two or more lists together allows uneven lists"   
  ([& cols]
     (lazy-seq
      (when-let [s (seq cols)]
        (cons (apply vector (map* first s)) (apply map* vector (map rest s)))))))


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
     (count (for [item col :when (pred item)]
       item))))

(defn count-occurances
  "Count number of occurances in a list of specified item"
  ([item col]
     (count-if (partial = item) col)))

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
     (for [[idx elem] (enumerate col)
	   :when (= elem el)]
       idx)))
	   
       
(defn list-tabulate
  "Create a list by calling the provided func with the position as a parameter"
  ([n func]
     (for [i (range n)]
       (func i))))


(defn flatten-1
  "Flatten the list but only up to the depth of one"
  ([col]
     (with-local-vars [result nil]
       (doseq [term col]
	 (if (seq? term)
	   (var-set result (concat @result term))
	   (var-set result (concat @result (list term)))))
	 @result)))
		 
	 

(defn vector-map
  "Vector equivalent of map"
  ([fn & seq]
     (loop [result []
	    seq seq]
       (if (some empty? seq)
	 result
	 (recur (conj result (apply fn (map first seq)))
		(map rest seq))))))
	    
(defn vector-filter
  "Vector equivalent of filter"
  ([fn seq]
     (loop [result []
	    seq seq]
       (if (empty? seq)
	 result
	 (recur (if (fn (first seq)) (conj result (first seq)) result)
		(rest seq))))))
     
(defn vector-tabulate
  "Create a vector based on call func for each position with the index
as a parameter i.e. (vector-map 3 f) => [(f 0) (f 1) (f 2)]"
  ([n func]
     (vector-map func (range n))))

(defn list->vector
  "Convert a list to a vector"
  ([col]
     (apply vector (seq col))))

(defn min-max
  "Calculate min and max of a sequence"
  ([col]
     (loop [col col
	    cur-min nil
	    cur-max nil]
       (if (nil? col)
	 [cur-min cur-max]
	 (let [[f & r] col]
	   (cond (or (nil? cur-min) (< f cur-min))
		 (recur r f cur-max)
		 (or (nil? cur-max) (> f cur-max))
		 (recur r cur-min f)
		 true
		 (recur r cur-min cur-max)))))))


	   
(defn freq
  "Calculate frequency of items in collection"
  ([col]
     (let [freqs (atom {})]
       (doseq [item col]
	 (let [f (get @freqs item 0)]
	   (swap! freqs assoc item (inc f))))
       @freqs)))
       


