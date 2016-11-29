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
    (:require [cljext.limits] [cljext.macros]) 
    (:refer cljext.limits) 
    (:refer cljext.macros)
    )

;;;; CONSTANTS

(def +pi+ Math/PI)
(def +e+ Math/E)
(def +NaN+ java.lang.Double/NaN)
(def +Inf+ java.lang.Double/POSITIVE_INFINITY)
(def +-Inf+ java.lang.Double/NEGATIVE_INFINITY)


;;;; PUBLIC

(defn to-integer
  "(to-integer v)
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
	 (int v)     
	 (> v +max-byte+)	 
	 (short v)     
	 true
	 (byte v))))

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
   (loop [result 1N
	  count exp]
     (if (zero? count)
       result
       (recur (* result base) (- count 1)))))
  ([exp]
   (ipow +e+ exp)))

(defn- round-even
  ([number]
  (if (integer? number)
    number
    (to-integer (Math/rint number)))))


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
    true (Math/pow base exp))))

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
   (/ (Math/log value)
      (Math/log base)))
  ([value] (Math/log value)))

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
   (Math/ceil val)))

(defn floor
  "(floor val)
Return the floor of a given number

val - numeric value

Returns:
double"
  ([val]
   (Math/floor val)))

;; (defn mod
;;   "(mod a n)
;; Modulo of a number with the sign of the divisor.  Similar to rem except 
;; result is same sign as divisor uses Knuth's floored division
;; a - numeric
;; n - numeric
;; returns:
;; numeric
;; "
;;   ([a n]
;;    (- a (* n (floor (/ a n))))))

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
	 true (reduce * (rest (range (+ n 1)))))))
			
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
   (Math/sin angle)))

(defn cos
  "(cos angle)
Take the cosine of an angle.

angle - numeric radians

Returns: 
double
"
  ([angle]
   (Math/cos angle)))

(defn tan
  "(tan angle)
Take the tangent of an angle.  Angle must be in radians.

angle - numeric radians

Returns:
double
"
  ([angle]
   (Math/tan angle)))

(defn asin
  "(asin angle)
Take the arc sine of an angle.

angle - numeric radians

Returns:
double
"
  ([angle]
   (Math/asin angle)))

(defn acos
  "(acos angle)
Take the arc sine of an angle.  

angle - numeric radians

Returns:
double

"
  ([angle]
   (Math/asin angle)))

(defn atan
  "(atan angle)
Take the arc tangent of an angle.  

angle - numeric radians

Returns:
double
"
  ([angle]
   (Math/asin angle)))

(defn- round-half-up
  ([number]
   (if (integer? number)
     number
     (let [x (mod (* (abs number) 10) 10)
	   sign (if (pos? number) 1 -1)]
       (if (>= x 5)
	 (to-integer (* sign (ceil (abs number))))
	 (to-integer (* sign (floor (abs number)))))))))
  
(defn- round-half-down
  ([number]
  (if (integer? number)
    number
    (let [x (mod (* (abs number) 10) 10)
	  sign (if (pos? number) 1 -1)]
      (if (> x 5)
	 (to-integer (* sign (ceil (abs number))))
	 (to-integer (* sign (floor (abs number)))))))))

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
  ([[term range] expr]
     (let [sum (gensym)
	   i (gensym)]
       `(loop [~sum 0
	       ~i ~range]
	  (if (empty? ~i)
	    ~sum
	    (let [~term (first ~i)]
	      (recur (+ ~sum ~expr) (rest ~i))))))))
     

;; operator precedence for formula macro
(def +precedence-table+ (ref {}))

;; symbol translation for symbols in formula 
;; (only supports binary operators)
(def +translation-table+ (ref {}))

(def +highest-precedence+ (ref 0))

(defn- defop
  "Define operators for formula macro"
  ([op prec & [trans]]
     (setq +precedence-table+ (assoc @+precedence-table+ op prec))
     (when-not (nil? trans)
       (setq +translation-table+ (assoc @+translation-table+ op trans)))
     (setq +highest-precedence+ (apply max (map val @+precedence-table+)))))


;; == operators ==
(defop '|| 10 'or)
(defop '&& 20 'and)
(defop '== 30 '=)
(defop '!= 30 'not=)
(defop '< 40)
(defop '> 40)
(defop '<= 40)
(defop '>= 40)
(defop '- 50)
(defop '+ 50)
(defop '/ 70)
(defop '* 70)
(defop 'mod 90 'rem)
(defop '** 100 'cljext.math/** )

(defn- operator?
  "Check if is valid operator"
  ([sym]
     (not (nil? (get @+precedence-table+ sym)))))

(defn- find-lowest-precedence
  "find the operator with lowest precedence; search from left to right"
  ([col]
     ;; loop through terms in the coluence
     (loop [idx 0
	    col col
	    lowest-idx nil
	    lowest-prec @+highest-precedence+]
       ;; nothing left to process
       (if (empty? col)
	 ;; return lowest found
	 lowest-idx
	 ;; otherwise check if current term is lower
	 (let [prec (get @+precedence-table+ (first col))]
	   ;; is of lower or equal precedence
	   (if (and prec (<= prec lowest-prec))
	     (recur (inc idx) (rest col)
		    idx prec)
	     ;; is of high precedence therefore skip for now
	     (recur (inc idx) (rest col)
		    lowest-idx lowest-prec)))))))

(defn- translate-op
  "Translation of symbol => symbol for binary op allows for
user defined operators"
  ([op] 
     (if (contains? @+translation-table+ op)
       (get @+translation-table+ op)
       op)))

(defn- infix-to-prefix
  "Convert from infix notation to prefix notation"
  ([col]
     (cond 
      ;; handle term only
      (not (seq? col)) col
      ;; handle sequence containing one term (i.e. handle parens)
      (= (count col) 1) (infix-to-prefix (first col))
      ;; handle all other cases
      true (let [lowest (find-lowest-precedence col)]
	     (if (nil? lowest) ;; nothing to split
	       col
	       ;; (a b c) bind a to hd, c to tl, and b to op
	       (let [[hd [op & tl]] (split-at lowest col)]
		 ;; recurse
		 (list (translate-op op)
		       (infix-to-prefix hd) 
		       (infix-to-prefix tl))))))))

(defmacro formula
  "Formula macro translates from infix to prefix"
  ([& equation]
     (infix-to-prefix equation)))

(defmacro product
  "(product term range expr)

Perform a product of a serias

term - binding for current term in range 
range - range 
expr - expression to multiply
"
  ([[term range] expr]
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
  ([col]
     (/ (apply + col) (count col))))

(defn sum
  "Sum a series"
  ([col]
     (apply + col)))

(defn geometric-mean
  "Geometric mean"
  ([col]
     (** (apply * col) (/ (count col)))))

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
			(formula (2 * t1 + t2) * t2)
			(formula (t1 * t1) + (t2 * t2)))))))))

	

(defn derivative 
  "Calculate the derivative

 (derivative func)
   return a function to calculate the derivative for a vlue of x

 (derivative func x h)
   calculate a derivative at a specific point
   returns double
"
  ;; return a function to calculate the derivative
  ([func]
   (fn [x & [h]]
       (derivative func x h)))
  ;; calculate the derivative value
  ([func x & [h]]
   (let [h (if (nil? h) 1e-8 h)]
     (/ (- (func (+ x h)) (func x)) h))))


;;;; PRIVATE


