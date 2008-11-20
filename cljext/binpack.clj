;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File     : binpack.clj
;; Function : Binary Packing
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

(ns cljext.binpack
    (:refer-clojure)
    (:require [cljext.limits])
    (:refer cljext.limits)
    )


;; Binpack is a binary packing and unpacking library similar to python's struct
;; library.  It provides two functions that are externally exposed.  
;;
;;  1) Pack - takes in a format string and parameter arguments and convets
;;            the arguments into bytes ready for output.  Signed numbers are
;;            represented in two's compliment when negative.
;;
;;  2) Unpack - is the inverse of pack.  Function takes in a format string and
;;              a sequence of bytes and converts it to a list of numbers.



;;;; SPECIAL

(def *endian*)

;;;; CONSTANTS

(def +little-endian+ 'little)
(def +big-endian+ 'big)


(def +conversion-table+
  {
  ;; spec char (size min max cast) (cast is needed for creating signed values)
  \b (list 1 +min-byte+ +max-byte+ byte),
  \B (list 1 +min-unsigned-byte+ +max-unsigned-byte+ identity),
  \h (list 2 +min-short+ +max-short+ short),
  \H (list 2 +min-unsigned-short+ +max-unsigned-short+ identity),
  \i (list 4 +min-integer+ +max-integer+ int),
  \I (list 4 +min-unsigned-integer+ +max-unsigned-integer+ identity),
  \l (list 8 +min-long+ +max-long+ long),
  \L (list 8 +min-unsigned-long+ +max-unsigned-long+ identity)
  })

;;;; PRIVATE

(defn- is-endian-spec?
  "(is-endian-spec? spec)
Check to see if a given character is a endian specifier

spec - character

Returns:
boolean
"
  ([spec]
   (contains? #{\> \! \<} spec)))

(defn- parse-endian 
  "(parse-endian? spec)
Return the endian based on the format string

spec - character

Returns:
symbol
"

  ([spec]
   (if (is-endian-spec? spec)
     (cond (or (= spec \>) (= spec \!))
	   +big-endian+
	   (= spec \<)
	   +little-endian+)
     +little-endian+)))

  
(defn- from-endian
  "(from-endian bytes cast)
Covert from a seq of bytes to a number

bytes - a sequence of bytes
cast - function to convert from output to the appropriate type

Returns:
numeric
"
  ([bytes cast]
   (let [bytes (if (= *endian* +little-endian+) bytes (reverse bytes))]
     (loop [bytes bytes
	    pos 0
	    result 0]
       ;; convert from unsigned rep to signed if required
       (if (empty? bytes)
	 (cast result)
	 ;; shift each byte to proper place
	 (let [byte (first bytes)]
	   (let [byte (if (< byte 0) (+ byte 256) byte)]
	     (recur (rest bytes) (inc pos) 
		    (+ result (bit-shift-left byte (* 8 pos)))))))))))

(defn- to-endian 
  "(to-endian n bytes)
Convert from a number to a sequence of bytes

n - number to convert
bytes - number of bytes to use

returns:
sequence of bytes
"
  ([n bytes]
   (let [neg (< n 0)]
     (loop [n n
	    bytes bytes
	    result nil]
       (if (= bytes 0)
	 ;; handle endians
	 (cond (= *endian* +big-endian+)
	       result
	       (= *endian* +little-endian+)
	       (reverse result))
	 ;; loop bit-shifting and storing LSB
	 (recur (bit-shift-right n 8)
		(dec bytes)
		(cons (bit-and n 0xFF)
		    result)))))))

(defn- in-range?
  "(in-range? x min max)
Check if x is between min and max (inclusive).  If not throw an exception

x - number to check
min - minimum allowable value
max - maximum allowable value

Returns:
true or throws exception
"
  ([x min max]
   (if (and (>= x min) (<= x max))
     true
     (throw 
      (new IllegalArgumentException 
	   (format "Argument value %d out of range [%d, %d]" x min max))))))

(defn- without-endian-spec 
  "(without-endian-spec fmt)
Remove first character if it is an endian specifier.

fmt - format string

Returns:
string
"
  ([fmt]
   (if (is-endian-spec? (first fmt)) (rest fmt) fmt)))


(defn- parse-stream 
  "(parse-stream fmt stream)

Break up a stream into groups of bytes associated with each format specifier.

fmt - format string
stream - sequence of bytes

Returns:
returns list of character and byte list pairs
"
  ([fmt stream]
   (loop [fmt fmt
	  stream stream
	  result nil]
     (if (empty? fmt)
       ;; end of format string
       (reverse result)
       (let [curfmt (first fmt)]
	 ;; handle format character
	 (if (contains? +conversion-table+ curfmt)
	   ;; handle conversion table entries
	   (let [size (first (get +conversion-table+ curfmt))]
	     (recur (rest fmt) (drop size stream) 
		    (cons (cons curfmt (take size stream)) result)))
	   ;; handle special characters
	   (cond (= curfmt \x)
	     (recur (rest fmt) (drop 1 stream) 
		    (cons (cons curfmt (take 1 stream)) result)))
	   ))))))
	   


;;; PUBLIC 

(defn pack [fmt & args]
  "(pack fmt & args)

Pack - takes in a format string and parameter arguments and convets
       the arguments into array bytes ready for output.  Signed numbers 
       are represented in two's compliment when negative.  The format is
       specified by the format string.

fmt - a format string
args - values to pack

Format strings:  

Format strings consist of an optional endian specifier followed one or
more type specifiers.

> - big endian
< - little endian (default)
! - network endian (big endian)

Type specifiers are a letter that specify the format of the appropriate 
argument for output.
b - signed byte
B - unsigned byte
h - signed short
H - unsigned short
i - signed int
I - unsigned int
l - signed long
L - unsigned long
x - padding byte (does not require argument)
Returns:
an array of bytes

Examples:

 (cljext.binpack/pack \">hh\" 33 -52)) => returns a byte array with contents
                                          (0 33 -1 -52)

 (cljext.binpack/pack \"<hh\" 33 -52)) => returns a byte array with contents
                                          (33 0 -52 -1)

 (cljext.binpack/pack \"<hxh\" 33 -52)) => returns a byte array with contents
                                          (33 0 0 -52 -1)

"
  (binding [*endian* (parse-endian (first fmt))]
    ;; loop through format string
    (loop [fmt (without-endian-spec fmt)
	   args args
	   result nil]
      (if (empty? fmt)
	;; convert result when nothing to loop through
	(into-array Byte/TYPE (map byte result))
	(let [c (first fmt) ;; format character
	      arg (first args)] ;; argument
	  (if (= c \x) ;; check if padding character
	    (recur (rest fmt) args (concat result [(byte 0)]))
	    ;; otherwise handle it from conversino table
	    (let [[size min max & body] (get +conversion-table+ c)]
	      (do (in-range? arg min max)
		  (recur (rest fmt) (rest args) 
			 (concat result (to-endian arg size)))
		  ))))))))

(defn unpack [fmt stream]
  "(unpack fmt stream)

fmt - a format string
stream - a sequence of signed bytes

Returns:
a list of values parsed from stream


Format strings:  

Format strings consist of an optional endian specifier followed one or
more type specifiers.

> - big endian
< - little endian (default)
! - network endian (big endian)

Type specifiers are a letter that specify the format of the appropriate 
argument for output.
b - signed byte
B - unsigned byte
h - signed short
H - unsigned short
i - signed int
I - unsigned int
l - signed long
L - unsigned long
x - padding byte (does not require argument)

Returns:
an array of bytes

Examples:

 (cljext.binpack/unpack \"<hh\" 
         (cljext.binpack/pack \"<hh\" 33 -52)) => (33 -52)

"

  (binding [*endian* (parse-endian (first fmt))]
    (loop [stream (parse-stream (without-endian-spec fmt) stream)
	   result []]
      ;; loop through stream
      (if (empty? stream)
	(reverse result)
	;; pull data out of conversion table
	(let [[type & bytes] (first stream)
	      [size min max cast] (if (= type \x) 
				    [1 0 0 int] 
				    (get +conversion-table+ type))
	      value (from-endian bytes cast)]
	  ;; check if padding character
	  (if (= type \x)
	    (recur (rest stream) result)
	    ;; handle standard conversion table entry
	    (recur (rest stream) (cons value  result))))))))
	
