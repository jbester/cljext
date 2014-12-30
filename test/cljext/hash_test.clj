(ns cljext.hash-test
    (:refer-clojure)
    (:require [cljext.hash :as hash]
	      [clojure.test :as test-is]))

(test-is/deftest test-hash-test
		 (let [md5 (hash/create-hash 'MD5)]
		   (test-is/is
		    (= (hash/hash-to-string 
			(do (md5 (.getBytes "Hello world"))
			    (md5)))
		       "3E25960A79DBC69B674CD4EC67A72C62")))
		 (let [crc (hash/create-hash 'CRC32)]
		   (test-is/is
		    (= (hash/hash-to-string 
			(do (crc (.getBytes "Hello world"))
			    (crc)))
		       "8BD69E52")))
		 )


(test-is/run-tests)


 
