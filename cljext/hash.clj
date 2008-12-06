;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File     : hash.clj
;; Function : hash library
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


;; Example:
;; user> (def md5 (create-hash 'MD5))
;; #'user/md5
;; user> (md5 (.getBytes "Hello world"))
;; nil
;; user> (hash-to-string (md5))
;; "3E25960A79DBC69B674CD4EC67A72C62"

(ns cljext.hash
  (:refer-clojure)
  (:require [cljext.macros] [cljext.seq])
  (:import [java.security.MessageDigest]
	   [java.io FileInputStream]
	   [java.util.zip CRC32 Adler32]))


;; supported hash algorithms

;; for internal use only
(def +digest-algorithms+ '(SHA1 MD2 MD5 SHA256 SHA384 SHA512))
;; valid algoirhtms
(def +hash-algorithms+ '(CRC32 Adler32 SHA1 MD2 MD5 SHA256 SHA384 SHA512))

(defn- hash-name
  "Convert from hash algorithm symbol to name required by getInstance 
of message digest"
  ([hash] 
     (str hash)))


(defn- make-hasher
  "Create appropriate class based on algorithm symbol"
  ([algorithm]
     (cond (cljext.seq/member? algorithm +digest-algorithms+)
	   (java.security.MessageDigest/getInstance (hash-name algorithm))
	   (= algorithm 'CRC32)
	   (java.util.zip.CRC32.)
	   (= algorithm 'Adler32)
	   (java.util.zip.Adler32.))))



(defn hash-file
  ([#^String filename algorithm & [buffer-size]]
     ;; create hasher and buffers
     (let [hash (create-hash algorithm)
	   buffer-size (if buffer-size buffer-size 1024)
	   buffer (make-array Byte/TYPE buffer-size)]
       ;; loop through file reading data from input
       (with-open [inp (java.io.FileInputStream. filename)]
	 (loop [rd (.read inp buffer)] 
	   ;; check if eof
	     (if (= rd -1)
	       (hash)
	       (do
		 ;; hash what was read and loop again
		 (hash (if (= rd buffer-size)
			  buffer
			  (subvec (apply vector (seq buffer)) 0 rd)))
		 (recur (.read inp buffer)))))))))
	   
	   
(defn create-hash
  "Create a hash for a given algorithm

Takes an algorithm as defined in +valid-algorithms+

Returns a function that takes either one or zero parameters.

If given one parameter a byte sequence the function will continue 
processing using the digest algorithm the sequence.

If given no parameters will return the hash as an array of bytes AND 
reset the hash algorithm to no input
"
  ([algorithm]
     (let [hasher
	   (make-hasher algorithm)]
       (when hasher
	 (fn 
	   ([]
	      (if (or (= algorithm 'CRC32) (= algorithm 'Adler32))
		(let [value (.getValue hasher)]
		  (.reset hasher)
		  [value])
		(.digest hasher)))
	   ([bytes]
	      (.update hasher (into-array Byte/TYPE (map byte bytes)))))))))

(defn- num->hex
  "Convert from base 10 to base 16"
  ([num]
     (format "%02X" num)))

(defn hash-to-string
  "Convert from byte array to string of hex digits"
  ([hash]
     (apply str (map num->hex hash))))

