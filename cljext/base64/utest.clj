(ns cljext.base64.utest
    (:refer-clojure)
    (:require [cljext.base64 :as b64]
	      [clojure.test :as test-is])
    )

(defn loopback
  ([str]
   (test-is/is 
    (= (b64/decode-string-as-string (b64/encode-string str)) str))))


(defn substring
  ([str start]
   (if (neg? start)
     (subs str (+ (count str) start)))))

(def +quote-1+ "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")
(def +quote-1-encoded+ "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")
(def +quote-2+ "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure")
(def +quote-3+ "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasur")

(test-is/deftest test-loopback
  (loopback +quote-1+) ;; test if invertible - one pad characters
  (loopback +quote-2+) ;; test invertble - two pad characters
  (loopback +quote-3+ ) ;; test invertble - no pad characters
  (test-is/is
   (= (substring (b64/encode-string +quote-1+) -2) "4=")) ;; test pad character is a =
  (test-is/is
   (= (substring (b64/encode-string +quote-2+) -3) "Q==")) ;; test pad two characters are ==
  (test-is/is
   (= (substring (b64/encode-string +quote-3+) -4) "c3Vy")) ;; test no extra characters
  (test-is/is 
   (= (b64/encode-string +quote-1+) ;; test against known output
      +quote-1-encoded+))
  )

(test-is/run-tests)
