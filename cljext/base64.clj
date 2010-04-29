;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File     : base64.clj
;; Function : Base64 Encoding Decoding
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

(ns cljext.base64
    (:refer-clojure)
    )

(def +base64+
     (concat
      (map char (range (int \A) (+ (int \A) 26)))
      (map char (range (int \a) (+ (int \a) 26)))
      (map char (range (int \0) (+ (int \0) 10)))
      (list \+ \/)))

(def +decode-map+
     (apply hash-map 
	    (interleave
	     +base64+
	     (range (count +base64+))
	     )))

(defn- bit-or*
  [& rest]
  (reduce bit-or rest))

(defn- encode-triplet
  "(encode-triplet a b c)

Convert a triplet to a quad of base64 encoded characters

a, b, c - byte of data

Returns:
4 base64 encoded characters
"
  ([a b c]
   (loop [i 0 ;; counter
	  bits (bit-or* (bit-shift-left a 16) (bit-shift-left b 8) c) ;; all bits concatenate together
	  result nil] 
     (let [sextuplet (bit-and bits 0x3F)] ;; take out lower 6 bits
       (if (= i 4)
	 result ;; end of loop
	 (recur ;; keep looping
	  (inc i)
	  (bit-shift-right bits 6) 
	  (cons (nth +base64+ sextuplet) result)) ;; use 6 bits as loop up into the +base64+ 
	 )))))

(defn- encode-triplets
  "(encode-triplets triplets)

Convert a triplet to a quad of base64 encoded characters

triplets - list of list of three bytes e.g. ((1 2 3) (4 5 6) ...)
Returns:
list of list of 4 base64 encoded characters 
"
  ([triplets]
   ;; split each triplet into 3 characters
   (for [[a b c] triplets]
     ;; for each triplet convert into 4 bytes
     (encode-triplet a b c))))



	       

(defn encode-string
  "(encode binary)

Convert to a base64 encoded string

binary - string or a sequence of bytes

Returns:
string base64 encoded 
"
  ([binary]
   (let [len (count binary)
	 remainder (rem len 3)
	 pad (if (zero? remainder) 0 (- 3 remainder))
	 ;; pad out with 0's in the mean time
	 triplets (partition 3 (concat (map int binary) (replicate pad 0 )))
	 ;; convert triplets to bytes
	 bytes (encode-triplets triplets)]
     ;; pull out the last byte and modify the the padding to be '=;
     (let [end-byte (concat (drop-last pad (last bytes))  (replicate pad \=))
	   ;; all but the last byte
	   bytes (apply concat (drop-last 1 bytes))]
       ;; convert to a string
       (apply str (concat bytes end-byte))))))

(defn encode-file
  "Encode a file in base64 and output to a given file

in-file-name - a string path for the input file name
out-file-name - a string path for the output file name
buffer-size - size of a buffer must be a multiple of 3

Returns:
true
"
  ([in-file-name out-file-name & [buffer-size]]
     (with-open [inp (java.io.FileInputStream. #^String in-file-name)
		 outp (java.io.FileOutputStream. #^String out-file-name)]
       ;; loop through files reading from one and writing to the other
       (let [buffer-size (if buffer-size buffer-size 1020)
	     buffer (make-array Byte/TYPE buffer-size)]
	 ;; looping 
	 (loop [rd (.read inp buffer)]
	   (if (= rd -1)
	     true
	     (do
	       ;; write to output file 
	       (if (= rd buffer-size)
		 (.write outp (.getBytes #^String (encode-string buffer)))
		 (.write outp (.getBytes #^String (encode-string  (subvec (apply vector (seq buffer)) 0 rd)))))
	       ;; keep looping
	       (recur (.read inp buffer)))))))))


(defn- decode-bytes
  "(decode-bytes bits c)

Decode a set of bits into c bytes

bits - binary
c - # of bytes to decode

Returns:
list of bytes
"
  ([bits c]
   ;; loop through extracting lower byee and storing it
   (loop [i 0 result nil]
     (let [byte (bit-and (bit-shift-right bits (* i 8)) 0xFF)]
       (if (= i c)
	 result
	 (recur (inc i) (cons byte result)))))))
  
(defn- decode-quad
  "(decode-quad a b c d)
 (decode-quad a b c)
 (decode-quad a b)

Decodes base64 character quads.

a b c d - base64 encoded characters

Returns:
list of decoded bytes"
  ([a b c d]
   (let [bits (bit-or* (bit-shift-left a 18) (bit-shift-left b 12) (bit-shift-left c 6) d)]
     (decode-bytes bits 3)))
  ([a b c]
   (let [bits (bit-shift-right (bit-or* (bit-shift-left a 12) (bit-shift-left b 6) c) 2)]
     (decode-bytes bits 2)))
  ([a b]
   (let [bits (bit-shift-right (bit-or* (bit-shift-left a 6) b) 4)]
     (decode-bytes bits 1))))

(defn- decode-quads
  "(decode-quads quads)

Decode a list of quads into a list of decoded bytes

quads - list of list of 4 base64 encoded bytes

Returns:
list of upto 3 bytes
"
  ([quad]
   (loop [q quad
	  result nil]
     (if (or (empty? q) (= (first q) \=))
       (apply decode-quad (reverse result))
       (recur (rest q) (cons (second (find +decode-map+ (first q))) result))))))



(defn decode-string
  "(decode-string string)

Takes in a base64 encoded string and converts to base64 decoded bytes

string - base64 encoded string

Returns:
list of bytes
"
  ([string]
   (let [quads (partition 4 string)]
     (apply concat (for [quad quads]
		     (decode-quads quad))))))
				  

(defn decode-string-as-string
  "(decode-string-as-string string)

decode a base64 string as a string

 string - base 64 encoded string

Returns:
decoded string 
"
  ([string]
   (new String #^bytes (into-array Byte/TYPE (map byte (decode-string string))))))
