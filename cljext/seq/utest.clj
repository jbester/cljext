(ns cljext.seq.utest
    (:refer-clojure)
    (:require [cljext.seq :as seq]
	      [clojure.test :as test-is]))


(test-is/deftest test-vector-map
   (test-is/is
    (= (seq/vector-map vector '(1 2) '(3 4 5))
       [[1 3] [2 4]]))
  )

(test-is/deftest test-vector-map*
   (test-is/is
    (= (seq/vector-map* vector '(1 2) '(3 4 5))
       [[1 3] [2 4] [nil 5]]))
  )


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

(test-is/deftest test-vector-tabulate
		 (test-is/is
		  (= (seq/vector-tabulate 10 inc)
		     '[1 2 3 4 5 6 7 8 9 10]))
		 )

(test-is/deftest test-vector-map
		 (test-is/is
		  (= (seq/vector-map inc (range 10))
		     '[1 2 3 4 5 6 7 8 9 10]))

		 (test-is/is
		  (= (seq/vector-map vector '(a b c) (range 10))
		     '[[a 0] [b 1] [c 2]]))

		 )

(test-is/deftest test-vector-filter
		 (test-is/is
		  (= (seq/vector-filter even? (range 10))
		     (apply vector (filter even? (range 10)))))

		 )

(test-is/deftest test-list->vector
		 (test-is/is
		  (= (seq/list->vector (range 10))
		     [0 1 2 3 4 5 6 7 8 9]))
		 )

(test-is/deftest test-min-max
		 (test-is/is
		  (= (seq/min-max '(-5 10 3 4 7 8 9))
		     [-5 10]))
		 )

(test-is/deftest test-freq
		 (test-is/is
		  (= (seq/freq '(a b c d a c b b))
		     {'a 2 'b 3 'c 2 'd 1}))
		 )


(test-is/run-tests)


 
