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

(def +CRC32+ 'CRC32)
(def +Adler32+ 'Adler32)
(def +SHA1+ 'SHA1)
(def +MD2+ 'MD2)
(def +MD5+ 'MD5)
(def +SHA256+ 'SHA256)
(def +SHA384+ 'SHA384)
(def +SHA512+ 'SHA512)

(defn- is-checksum?
  ([algo]
     (or (= algo +CRC32+) (= algo +Adler32+))))

(defn- is-digest?
  "Internally used to check if algorithm is supported by java's digest message class"
  ([algo]
     (not (is-checksum? algo))))

(defn- hash-name
  "Convert from hash algorithm symbol to name required by getInstance 
of message digest"
  ([hash] 
     (str hash)))


(defn- make-hasher
  "Create appropriate class based on algorithm symbol"
  ([algorithm]
     (cond (is-digest? algorithm)
	   (java.security.MessageDigest/getInstance (hash-name algorithm))
	   (= algorithm 'CRC32)
	   (java.util.zip.CRC32.)
	   (= algorithm 'Adler32)
	   (java.util.zip.Adler32.))))

(defn bit-shift-right-and-mask
  ([v n mask]
     (bit-and (bit-shift-right v n) mask)))

(defn- digest-checksum
  ([hasher]
     (let [value (.getValue #^java.util.zip.Checksum hasher)]
       (.reset #^java.util.zip.Checksum hasher)
       [(bit-shift-right-and-mask value 24 0xFF) (bit-shift-right-and-mask value 16 0xFF) (bit-shift-right-and-mask value 8 0xFF) (bit-shift-right-and-mask value 0 0xFF)])))

(defn- make-checksum-fn
  ([hasher]
     (fn 
       ([]
	  (digest-checksum hasher))
       ([bytes]
	  (if (seq? bytes)
	    (.update hasher #^"[B" (into-array Byte/TYPE (map byte bytes)))
	    (.update hasher #^"[B" bytes))))))


(defn- make-digest-fn
  ([hasher]
     (fn 
       ([]
	  (.digest #^java.security.MessageDigest hasher))
       ([bytes]
	  (if (seq? bytes)
	    (.update #^java.security.MessageDigest hasher #^bytes (into-array Byte/TYPE (map byte bytes)))
	    (.update #^java.security.MessageDigest hasher #^bytes bytes))))))
      


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
	 (if (is-digest? algorithm)
	   (make-digest-fn hasher)
	   (make-checksum-fn hasher))))))


(defn hash-file
  "Hash a file 

filename - path as a String
algorithm - algorithm to use as a symbol (e.g. 'CRC32)
buffer-size - (optional) size of buffer to use default is 1KB

Returns:
Hash as seq of bytes
"
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
			  (subvec (cljext.seq/list->vector buffer) 0 rd)))
		 (recur (.read inp buffer)))))))))
	   
	   

(defn- num->hex
  "Convert from base 10 to base 16"
  ([num]
     (format "%02X" num)))

(defn hash-to-string
  "Convert from byte array to string of hex digits"
  ([hash]
     (apply str (map num->hex hash))))

