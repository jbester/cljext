(ns cljext.str-test
    (:refer-clojure)
    (:require [cljext.str :as str]
              [clojure.test :as test-is]))

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

(test-is/deftest test-concat
   (test-is/is
    (= (str/str-concat "hello" " world")
       "hello world"))
   (test-is/is
    (= (str/str-concat "hello" " world" " byte")
       "hello world byte")))

 


(test-is/run-tests)
