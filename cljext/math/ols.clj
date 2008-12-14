;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File     : ols.clj
;; Function : Ordinary least squares
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

(ns cljext.math.ols
  (:refer-clojure)
  (:require [cljext.math]
	    [cljext.seq]
	    [cljext.macros])
  (:refer cljext.math)
  (:refer cljext.seq)
  (:refer cljext.macros))

(defn ols
  "(ols points)
Ordinary Least Squares

points - a list of datapoints in the form (x,y) e.g. [(1,2),(2,4),(4,8)]

Returns:
  (slope est, intercept est )
"
  ([points]
   (let [[x_points y_points] (unzip points)
	 len (count points)
	 sum_x (apply + x_points)
	 sum_y (apply + y_points)
	 ;; average x and y
	 mean_x (/ sum_x len)
	 mean_y (/ sum_y len)]
     (with-local-vars [sum_xx 0 
		       sum_xy 0
		       sum_yy 0]
       (do
	 ;; sums for each deviation from th emean
	 (doseq [[x y] points]
	   (let [x_dev (- x mean_x)
		 y_dev (- y mean_y)]
	     (var-set sum_xx (+ (var-get sum_xx) (* x_dev x_dev)))
	     (var-set sum_xy (+ (var-get sum_xy) (* x_dev y_dev)))
	     (var-set sum_yy (+ (var-get sum_yy) (* y_dev y_dev)))))
	 ;; calculate beta, alpha, and corr
	 (let [beta (/ (var-get sum_xy) (var-get sum_xx))
	       alpha (- mean_y (* beta mean_x))
	       corr (/ (* (var-get sum_xy) (var-get sum_xy)) (* (var-get sum_xx)  (var-get sum_yy)))]
	   [ beta, alpha]))))))

