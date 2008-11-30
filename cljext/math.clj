;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File     : math.clj
;; Function : Math library
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

(ns cljext.math
    (:refer-clojure)
    (:require [cljext.limits]) 
    (:refer cljext.limits) 
    )

;;;; CONSTANTS

(def +pi+ (.PI Math))
(def +e+ (.E Math))
(def +NaN+ (.NaN Double))
(def +Inf+ (.POSITIVE_INFINITY Double))
(def +-Inf+ (.NEGATIVE_INFINITY Double))


;;;; PUBLIC


;;;; PRIVATE

(defn- ipow
  "(ipow base exp) 
Performs exponention if exp is positive integer. 

base - numeric base to raise
exp - integer power to raise

Returns:
base ** exp

  (ipow exp)
Performs e ** exp if exp is a positive integer

exp - integer power to raise e to

Returns:
numeric"
  ([base exp]
   (loop [result 1
	  count exp]
     (if (zero? count)
       result
       (recur (* result base) (dec count)))))
  ([exp]
   (ipow +e+ exp)))



(defn- round-even
  ([number]
  (if (integer? number)
    number
    (.rint Math number))))


;;;; PUBLIC

(defn integer
  "(integer v)
Coerce to an integer of the smallest type possible

v - numeric

Returns:
integer numeric
"
  ([v]
   (cond (> v +max-long+)
	 (bigint v)
	 (> v +max-integer+)
	 (long v)
	 (> v +max-short+)	 
	 (integer v)     
	 (> v +max-byte+)	 
	 (short v)     
	 true
	 (byte v))))

(defn bigdec?
  "(bigdec? v)
test if bigdecimal

v - any value

Returns:
bool"
  ([v]
   (= (class v) java.math.BigDecimal)))

(defn bigint?
  "(bigint? v)
test if biginteger

v - any value

Returns: 
true if bigint otherwise false"
  ([v]
   (= (class v) java.math.BigInteger)))

(defn numeric?
  "(numeric? v)
Check to see if v is a numeric type

v - any value

Returns:
bool
"
  ([v]
   (or (integer? v)
       (float? v)
       (bigdec? v)
       (ratio? v ))))



(defn abs
  "(abs n) 
Absolute value of a number

n - numeric value

Returns:
numeric
"
  [n]
  (if (not (neg? n))
    n
    (* -1 n)))
 

(defn **
  "(** base exp)
Performs exponenation

base - numeric base to raise
exp - numeric power to raise base to

Returns:
numeric"
  ([base exp]
   (cond 
    (and (integer? exp) (not (neg? exp))) (ipow base exp)
    (and (integer? exp) (neg? exp)) (/ (ipow base (abs exp)))
    true (.pow Math base exp))))

(defn log 
  "(log base value)
Log of a given base of a value 

base - numeric logrithmic base
value - numeric value to take the logarithm of

Returns: 
double

   (log value)
value - numeric value to take the natural logarithm of

Returns: 
double
"
  ([base value] 
   (/ (.log Math value)
      (.log Math base)))
  ([value] (.log Math value)))

(defn log10
  "(log10 value)
Perform log base 10

value - numeric value to take the log base 10 of

Returns: 
double"
  ([value]
   (log 10 value)))

(defn exp
  "(exp power)
Raise E to a given power

power - numeric power to raise E to

Returns: 
double"

  ([power]
   (ipow power)))

(defn sqrt
  "(sqrt number)
Take the square root of a given number

number - numeric value to take the square root of

Returns:
numeric
"
  ([number]
   (** number 0.5)))

(defn root
  "(root rt number)
Take an arbitrary root of a number

rt - numeric value of root
number - numeric value to take the root 

Returns:
numeric
"
  ([rt number]
   (** number (/ rt))))

(defn ceil
  "(ceil val)
Return the ceiling of a given number

val - numeric value

Returns:
double
"
  ([val]
   (.ceil Math val)))

(defn floor
  "(floor val)
Return the floor of a given number

val - numeric value

Returns:
double"
  ([val]
   (.floor Math val)))

(defn mod
  "(mod a n)
Modulo of a number with the sign of the divisor.  Similar to rem except 
result is same sign as divisor uses Knuth's floored division

a - numeric
n - numeric

returns:
numeric
"
  ([a n]
   (- a (* n (floor (/ a n))))))

(defn factorial
  "(factorial n)
Calculate the factorial

n - positive integer or 0

Returns:
integer factorial"
  ([n]
   (cond (neg? n) (throw 
		   (new java.lang.IllegalArgumentException
			"Cannot perform factorial of a negative number"))
	 (zero? n) 1
	 true (loop [result 1
		     i n]
		(if (= i 1)
		  result
		  (recur (* result i)
			 (dec i)))))))
			
(defn to-degrees
  "(to-degrees rads)
Convert from radians to degrees

rads - numeric radians

Returns:
numeric
"
  ([rads]
   (/ (* rads 180) +pi+)))


(defn to-radians
  "(to-radians degrees)
Convert from degrees to radians

degrees - numeric degrees

Returns:
numeric
"
  ([degrees]
   (* (/ degrees 180) +pi+)))
  
(defn sin
  "(sin angle)
Take the sine of an angle.  

angle - numeric radians

Returns:
double
"
  ([angle]
   (.sin Math angle)))

(defn cos
  "(cos angle)
Take the cosine of an angle.

angle - numeric radians

Returns: 
double
"
  ([angle]
   (.cos Math angle)))

(defn tan
  "(tan angle)
Take the tangent of an angle.  Angle must be in radians.

angle - numeric radians

Returns:
double
"
  ([angle]
   (.tan Math angle)))

(defn asin
  "(asin angle)
Take the arc sine of an angle.

angle - numeric radians

Returns:
double
"
  ([angle]
   (.asin Math angle)))

(defn acos
  "(acos angle)
Take the arc sine of an angle.  

angle - numeric radians

Returns:
double

"
  ([angle]
   (.asin Math angle)))

(defn atan
  "(atan angle)
Take the arc tangent of an angle.  

angle - numeric radians

Returns:
double
"
  ([angle]
   (.asin Math angle)))

(defn- round-half-up
  ([number]
   (if (integer? number)
     number
     (let [x (mod (* (abs number) 10) 10)
	   sign (if (pos? number) 1 -1)]
       (if (>= x 5)
	 (* sign (ceil (abs number)))
	 (* sign (floor (abs number))))))))
  
(defn- round-half-down
  ([number]
  (if (integer? number)
    number
    (let [x (mod (* (abs number) 10) 10)
	  sign (if (pos? number) 1 -1)]
      (if (> x 5)
	 (* sign (ceil (abs number)))
	 (* sign (floor (abs number))))))))

(defn round
  "(round number)
Round to even

number - numeric value

returns:
integer

   (round number type)
Round per specified rounding type

number - numeric value
rounding - symbols one of the following: 'half-up 'half-down 'even

returns:
integer
"
  ([number]
   (round-even number))
  ([number type]
   (cond (= type 'half-up)
	 (round-half-up number)
	 (= type 'half-down)
	 (round-half-down number)
	 (= type 'even)
	 (round-even number))))



(defmacro summation
  "(summation term range expr)

Perform a summation

term - binding for current term in range 
range - range 
expr - expression to sum
"
  ([term range expr]
     (let [sum (gensym)
	   i (gensym)]
       `(loop [~sum 0
	       ~i ~range]
	  (if (empty? ~i)
	    ~sum
	    (let [~term (first ~i)]
	      (recur (+ ~sum ~expr) (rest ~i))))))))
     


(defmacro product
  "(product term range expr)

Perform a product of a serias

term - binding for current term in range 
range - range 
expr - expression to multiply
"
  ([term range expr]
     (let [prod (gensym)
	   i (gensym)]
       `(loop [~prod 1
	       ~i ~range]
	  (if (empty? ~i)
	    ~prod
	    (let [~term (first ~i)]
	      (recur (* ~prod ~expr) (rest ~i))))))))
     
(defn gcd 
  "Greatest common denominator

Euclid's algorithm
GCD(A,B)==GCD(B,A%B)
"
  ([a b]
     (if (zero? b)
       a
       (recur b (rem a b)))))

(defn lcm
  "Least common multiple"
  ([a b]
     (/ (* a b) (gcd a b))))

(defn nPr
  "Permutations"
  ([n r]
     (/ (factorial n) (factorial (- n r)))))

(defn nCr
  "Combinations"
  ([n r]
     (/ (factorial n) (* (factorial r) (factorial (- n r))))))

(defn mean
  "Calculate mean"
  ([seq]
     (/ (apply + seq) (count seq))))

(defn geometric-mean
  "Geometric mean"
  ([seq]
     (** (apply * seq) (/ (count seq)))))

(defn fibonacci
  "Fibonacci sequence"
  ([n]
     (cond (= n 0) 0
	   (= n 1) 1
	   ;; Uses Djikstra's recursion
	   ;; F(2n) = (2 F(n-1) + F(n)) * F(n)
	   ;; F(2n-1) = F(n-1)^2 + F(n)^2
	   true (let [limit (ceil (/ n 2))]
		  (loop [t1 0
			 t2 1
			 pos 2]
		    (if (<= pos limit)
		      (let [tmp (+ t1 t2)]
			(recur t2 tmp (inc pos)))
		      (if (even? n)
			(* (+ (* 2 t1) t2) t2)
			(+ (* t1 t1) (* t2 t2)))))))))
	

;;pemdas
(def +precedence+
     {'** 5,
      '* 4,
      '/ 3,
      '+ 2,
      '- 1})

(def +highest-precedence+ 5)

(defn- operator?
  "Check if is valid operator"
  ([sym]
     (not (nil? (get +precedence+ sym)))))

(defn find-lowest-precedence
  "find the operator with lowest precedence; search from left to right"
  ([seq]
     ;; loop through terms in the sequence
     (loop [idx 0
	    seq seq
	    lowest-idx nil
	    lowest-prec +highest-precedence+]
       ;; nothing left to process
       (if (empty? seq)
	 ;; return lowest found
	 lowest-idx
	 ;; otherwise check if current term is lower
	 (let [prec (get +precedence+ (first seq))]
	   ;; is of lower or equal precedence
	   (if (and prec (<= prec lowest-prec))
	     (recur (inc idx) (rest seq)
		    idx prec)
	     ;; is of high precedence therefore skip for now
	     (recur (inc idx) (rest seq)
		    lowest-idx lowest-prec)))))))

(defn infix-to-prefix
  "Convert from infix notation to prefix notation"
  ([seq]
     (cond 
      ;; handle term only
      (not (seq? seq)) seq
      ;; handle sequence containing only one term
      (= (count seq) 1) (first seq)
      ;; handle all other cases
      true (let [lowest (find-lowest-precedence seq)]
	     (if (nil? lowest) ;; nothing to split
	       seq
	       ;; (a b c) bind a to hd, c to tl, and b to op
	       (let [[hd tl] (split-at lowest seq)
		     op (first tl)
		     tl (rest tl)]
		 ;; recurse
		 (list op (infix-to-prefix hd) (infix-to-prefix tl))))))))

(defmacro formula
  "Formula macro translates from infix to prefix"
  ([& equation]
     (infix-to-prefix equation)))