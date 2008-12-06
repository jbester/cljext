(ns cljext.seq.utest
    (:refer-clojure)
    (:require [cljext.seq :as seq]
	      [clojure.contrib.test-is :as test-is]))


(test-is/deftest test-map*
   (test-is/is
    (= (seq/map* vector '(1 2) '(3 4 5))
       '([1 3] [2 4] [nil 5])))
  )

(test-is/deftest test-zip
		 (test-is/is
		  (= (seq/zip '(1 2 3) '(4 5 6))
		     '([1 4] [2 5] [3 6])))
		 (test-is/is
		  (= (seq/zip '(1 2 3) '(4 5 6) '(7 8 9))
		     '([1 4 7] [2 5 8] [3 6 9])))
		 )


(test-is/deftest test-zip*
		 (test-is/is
		  (= (seq/zip* '(1 2 3) '(4 5))
		     '([1 4] [2 5] [3 nil])))
		 (test-is/is
		  (= (seq/zip* '(1 2 3) '(4 5 6))
		     '([1 4] [2 5] [3 6])))

		 (test-is/is
		  (= (seq/zip* '(1 2 3) '(4 5 6) '(7 8 9))
		     '([1 4 7] [2 5 8] [3 6 9])))
		 )


(test-is/deftest test-lazy-zip
		 (test-is/is
		  (= (seq/lazy-zip '(1 2 3) '(4 5 6))
		     '([1 4] [2 5] [3 6])))
		 (test-is/is
		  (= (seq/lazy-zip '(1 2 3) '(4 5 6) '(7 8 9))
		     '([1 4 7] [2 5 8] [3 6 9])))
		 )

(test-is/deftest test-lazy-zip*
		 (test-is/is
		  (= (seq/lazy-zip* '(1 2 3) '(4 5 6))
		     '([1 4] [2 5] [3 6])))
		 (test-is/is
		  (= (seq/lazy-zip* '(1 2 3) '(4 5 6) '(7 8 9))
		     '([1 4 7] [2 5 8] [3 6 9])))
		 )

(test-is/deftest test-unzip
		 (test-is/is
		  (= (seq/unzip '((1 2) (3 4) (5 6)))
		     '((1 3 5) (2 4 6))))

		 (test-is/is
		  (= (seq/unzip '((1 2 5) (3 4 5) (5 6 4)))
		     '((1 3 5) (2 4 6) (5 5 4))))

		 )

(test-is/deftest test-enumerate
		 (test-is/is
		  (= (seq/enumerate '(a b c d e f))
		     '([0 a] [1 b] [2 c] [3 d] [4 e] [5 f])))
		 )

(test-is/deftest test-count-if
		 (test-is/is
		  (= (seq/count-if identity (range 10))
		     (count (range 10))))
		 )

(test-is/deftest test-count-occurances
		 (test-is/is
		  (= (seq/count-occurances 'a '(a b c d a f g a))
		     3))
		 )

(test-is/deftest test-member?
		 (test-is/is
		  (= (seq/member? 'a '(d e f a b c))
		     'a))
		 )


(test-is/deftest test-member?
		 (test-is/is
		  (= (seq/positions 'a '(a b c d a f g a))
		     '(0 4 7)))
		 )


(test-is/deftest test-list-tabulate
		 (test-is/is
		  (= (seq/list-tabulate 10 inc)
		     '(1 2 3 4 5 6 7 8 9 10)))
		 )


(test-is/run-tests)


 
