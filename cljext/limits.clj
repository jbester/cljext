;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File     : limits.clj
;; Function : Numeric limits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Newest version can be obtained at http://www.freshlime.org
;; Send comments or questions to code at freshlime dot org
;; $Id: limits.clj 4 2008-11-16 21:20:23Z jbester $
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

(ns cljext.limits
    (:refer-clojure)
    )

(def +max-unsigned-long+ 0xFFFFFFFFFFFFFFFF)
(def +min-unsigned-long+ 0x00)
(def +max-long+ java.lang.Long/MAX_VALUE)
(def +min-long+ java.lang.Long/MIN_VALUE)
(def +max-unsigned-integer+ 0xFFFFFFFF)
(def +min-unsigned-integer+ 0x00)
(def +max-integer+ java.lang.Integer/MAX_VALUE)
(def +min-integer+ java.lang.Integer/MIN_VALUE)
(def +max-unsigned-short+ 0xFFFF)
(def +min-unsigned-short+ 0x00)
(def +max-short+ java.lang.Short/MAX_VALUE)
(def +min-short+ java.lang.Short/MIN_VALUE)
(def +max-unsigned-byte+ 0xFF)
(def +min-unsigned-byte+ 0x00)
(def +max-byte+ java.lang.Byte/MAX_VALUE)
(def +min-byte+ java.lang.Byte/MIN_VALUE)
