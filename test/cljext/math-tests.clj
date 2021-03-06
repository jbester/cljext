(ns cljext.math.utest
    (:refer-clojure)
    (:require [cljext.math :as math]
	      [clojure.test :as test-is]))

(test-is/deftest test-abs
  ;; test neg -> pos			
  (test-is/is 
   (true? (= (math/abs -10) 10)))
  (test-is/is 
   (true? (= (math/abs (float -1.4)) (float 1.4))))
  (test-is/is 
   (true? (= (math/abs -3/4) 3/4)))
  (test-is/is 
   (true? (= (math/abs (long -10)) (long 10))))
  (test-is/is 
   (true? (= (math/abs (double -10.4)) (double 10.4))))
  (test-is/is 
   (true? (= (math/abs (bigdec (long -10))) (bigdec (long 10)))))
  (test-is/is 
   (true? (= (math/abs (bigint (long -10))) (bigint (long 10)))))
  ;; test pos -> pos
  (test-is/is 
   (true? (= (math/abs 10) 10)))
  (test-is/is 
   (true? (= (math/abs (float 1.4)) (float 1.4))))
  (test-is/is 
   (true? (= (math/abs 3/4) 3/4)))
  (test-is/is 
   (true? (= (math/abs (long 10)) (long 10))))
  (test-is/is 
   (true? (= (math/abs (double 10.4)) (double 10.4))))
  (test-is/is 
   (true? (= (math/abs (bigdec (long 10))) (bigdec (long 10)))))
  (test-is/is 
   (true? (= (math/abs (bigint (long 10))) (bigint (long 10)))))
  )

(test-is/deftest test-pi
  ;; 
  (test-is/is 
   (true? (= (Math/sin (/ math/+pi+ 2)) 1.0))))

(test-is/deftest test-e
  ;; 
  (test-is/is 
   (true? (= (Math/log math/+e+) 1.0))))


(test-is/deftest test-round
  ;; half up to even
  (test-is/is 
   (true? (= (math/round 1.5) 2)))
  ;; negative
  (test-is/is 
   (true? (= (math/round -0.5) 0)))
  ;; half down to even
  (test-is/is 
   (true? (= (math/round 2.5) 2)))
  ;; less than half to lower odd
  (test-is/is 
   (true? (= (math/round 1.1) 1)))
  ;; less than half to lower even
  (test-is/is 
   (true? (= (math/round 2.1) 2)))
  ;; more than half up to odd
  (test-is/is 
   (true? (= (math/round 2.9) 3)))
  ;; more than half up to even
  (test-is/is 
   (true? (= (math/round 1.9) 2)))

  ;; half up to even
  (test-is/is 
   (true? (= (math/round 1.5 'half-up) 2)))
  ;; half up to odd
  (test-is/is 
   (true? (= (math/round 2.5 'half-up) 3)))
  ;; less than half
  (test-is/is 
   (true? (= (math/round 2.2 'half-up) 2)))
  ;; more than half
  (test-is/is 
   (true? (= (math/round 2.6 'half-up) 3)))
  ;; half negative
  (test-is/is 
   (true? (= (math/round -2.5 'half-up) -3)))
  ;; more than half negative
  (test-is/is 
   (true? (= (math/round -2.6 'half-up) -3)))
  ;; less than half negative
  (test-is/is 
   (true? (= (math/round -2.4 'half-up) -2)))


  ;; half up to even
  (test-is/is 
   (true? (= (math/round 1.5 'half-down) 1)))
  ;; half up to odd
  (test-is/is 
   (true? (= (math/round 2.5 'half-down) 2)))
  ;; less than half
  (test-is/is 
   (true? (= (math/round 2.2 'half-down) 2)))
  ;; more than half
  (test-is/is 
   (true? (= (math/round 2.6 'half-down) 3)))
  ;; half negative
  (test-is/is 
   (true? (= (math/round -2.5 'half-down) -2)))
  ;; more than half negative
  (test-is/is 
   (true? (= (math/round -2.6 'half-down) -3)))
  ;; less than half negative
  (test-is/is 
   (true? (= (math/round -2.4 'half-down) -2)))



  )


(test-is/deftest test-**
  ;; normal
  (test-is/is 
   (true? (= 1 (math/** 1 1))))
  ;; normal 
  (test-is/is 
   (true? (= 1 (math/** 1 (bigint 1234)))))
  ;; bigint case
  (test-is/is 
   (true? (= 3755705223710067706661999915144080311060346600054404622065648816240972751638011600159252161527438966173298317722973493264712752371960832588164058262293839177998569490642059843982692168006801370509340322013837031886402093056 (math/** 1234 72))))
  ;; inverted case
  (test-is/is 
   (true? (= 1/3755705223710067706661999915144080311060346600054404622065648816240972751638011600159252161527438966173298317722973493264712752371960832588164058262293839177998569490642059843982692168006801370509340322013837031886402093056 (math/** 1234 -72))))
   ;; root case
  (test-is/is 
   (true? (= 1234 (math/round (math/** 3755705223710067706661999915144080311060346600054404622065648816240972751638011600159252161527438966173298317722973493264712752371960832588164058262293839177998569490642059843982692168006801370509340322013837031886402093056 1/72) 'half-up))))

  ;; root case double
  (test-is/is 
   (true? (= 1234 (math/round (math/** 3755705223710067706661999915144080311060346600054404622065648816240972751638011600159252161527438966173298317722973493264712752371960832588164058262293839177998569490642059843982692168006801370509340322013837031886402093056 (double 1/72)) 'half-up))))
  )

(test-is/deftest test-log
  (test-is/is 
   (true? (= (math/log math/+e+) 1.0)))
  (test-is/is 
   (true? (= (math/log 10 10) 1.0)))
  (test-is/is 
   (true? (= (math/log 10 100) 2.0)))
  (test-is/is 
   (true? (= (math/log 2 1024) 10.0)))

  )


(test-is/deftest test-log10
  (test-is/is 
   (true? (= (math/log10 10) 1.0)))
  (test-is/is 
   (true? (= (math/log10 100) 2.0)))
  (test-is/is 
   (true? (= (math/log10 92) 1.9637878273455551)))
  )

(test-is/deftest test-exp
  (test-is/is 
   (true? (= (math/exp 10) 22026.465794806707)))
  (test-is/is 
   (true? (= (math/exp 1) math/+e+)))
  )

(test-is/deftest test-sqrt
  (test-is/is 
   (true? (= (math/sqrt (math/exp 2)) math/+e+)))
  (test-is/is 
   (true? (= (math/sqrt 4) 2.0)))
  )

(test-is/deftest test-root
  (test-is/is 
   (true? (= (math/root 3 27) 3.0)))
  (test-is/is 
   (true? (= (math/root 2 4) 2.0)))
  (test-is/is 
   (true? (= (float (math/root 3 (math/exp 3))) (float  math/+e+))))
  )

(test-is/deftest test-ceil
  (test-is/is 
   (true? (= (math/ceil 3.4) 4.0)))
  (test-is/is 
   (true? (= (math/ceil -1.2) -1.0)))

  )

(test-is/deftest test-floor
  (test-is/is 
   (true? (= (math/floor 3.4) 3.0)))
  (test-is/is 
   (true? (= (math/floor -1.2) -2.0)))
  )

(test-is/deftest test-factorial
  (test-is/is 
   (true? (= (math/factorial 5) 120)))
  (test-is/is 
   (true? (= (math/factorial 0) 1)))
  (test-is/is (thrown? IllegalArgumentException (math/factorial -1)))
  )

(test-is/deftest test-to-degrees
  (test-is/is 
   (true? (= (math/to-degrees math/+pi+) 180.0)))
)

(test-is/deftest test-to-radians
  (test-is/is 
   (true? (= (math/to-radians 180.0) math/+pi+)))
  )


(test-is/deftest test-summation
  (test-is/is 
   (true? (= (math/summation [i (range 10)] i) (apply + (range 10)))))
  (test-is/is 
   (true? (= (math/summation [i (range 0 10 2)] i) (apply + (range 0 10 2)))))
  )

(test-is/deftest test-product
  (test-is/is 
   (true? (= (math/product [i (range 1 10)] i) (apply * (range 1 10)))))
  (test-is/is 
   (true? (= (math/product [i (range 1 10 2)] i) (apply * (range 1 10 2)))))
  )


(test-is/deftest gcd
  (test-is/is 
   (true? (= (math/gcd 50 30) 10)))
  )

(test-is/deftest lcm
  (test-is/is 
   (true? (= (math/lcm 5 3) 15)))
  )

(test-is/deftest test-permutations
  (test-is/is 
   (true? (= (math/nPr 5 3) 60)))
  )

(test-is/deftest test-combinations
  (test-is/is 
   (true? (= (math/nCr 5 3) 10)))
  )


(test-is/deftest test-fib
		 (test-is/is
		  (every? 
		   true? 
		   (map = (map cljext.math/fibonacci (range 21))
			  (list 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 
				987 1597 2584 4181 6765)))))


(test-is/deftest test-mean
  (test-is/is 
   (true? (= (math/mean '(10 4 3)) (/ (+ 10 4 3) 3))))
  )

(test-is/deftest test-geometric-mean
  (test-is/is 
   (true? (= (cljext.math/geometric-mean (list 1 1.04 1.0816)) 1.04)))
  )

(test-is/run-tests)


 
