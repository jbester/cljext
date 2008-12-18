(ns cljext.str.utest
    (:refer-clojure)
    (:require [cljext.str :as str]
              [clojure.contrib.test-is :as test-is]))

(test-is/deftest test-chomp
   (test-is/is
    (= (str/chomp "hello\r\n")
       "hello"))
   (test-is/is
    (= (str/chomp "hello\n")
       "hello"))
   (test-is/is
    (= (str/chomp "hello\r")
       "hello"))
   (test-is/is
    (= (str/chomp "hello")
       "hello")))
 


(test-is/run-tests)