(ns cljext.math.utest
    (:refer-clojure)
    (:require [cljext.math :as math]
	      [clojure.contrib.test-is :as test-is]))

(test-is/deftest test-bigdec?
  (test-is/is 
   (false? (math/bigdec? (bigint 1024))))
  (test-is/is 
   (false? (math/bigdec? 1024)))
  (test-is/is 
   (false? (math/bigdec? (long 1024))))
  (test-is/is 
   (false? (math/bigdec? (short 1024))))
  (test-is/is 
   (false? (math/bigdec? (byte 1024))))
  (test-is/is 
   (true? (math/bigdec? (bigdec 1024.4))))
  (test-is/is
   (false? (math/bigdec? (double 1024.4))))
  (test-is/is
   (false? (math/bigdec? (float 1024.4))))
  (test-is/is
   (false? (math/bigdec? 3/4)))
  (test-is/is
   (false? (math/bigdec? "hello")))
  )

(test-is/deftest test-bigint?
  (test-is/is 
   (true? (math/bigint? (bigint 1024))))
  (test-is/is 
   (false? (math/bigint? 1024)))
  (test-is/is 
   (false? (math/bigint? (short 1024))))
  (test-is/is 
   (false? (math/bigint? (byte 1024))))
  (test-is/is 
   (false? (math/bigint? (long 1024))))
  (test-is/is 
   (false? (math/bigint? (bigdec 1024.4))))
  (test-is/is
   (false? (math/bigint? (double 1024.4))))
  (test-is/is
   (false? (math/bigint? (float 1024.4))))
  (test-is/is
   (false? (math/bigint? 3/4)))
  (test-is/is
   (false? (math/bigint? "hello")))
  )

(test-is/deftest test-numeric?
  (test-is/is 
   (true? (math/numeric? (short 1024))))
  (test-is/is 
   (true? (math/numeric? (byte 1024))))
   (test-is/is 
    (true? (math/numeric? (bigint 1024))))
   (test-is/is 
    (true? (math/numeric? 1024)))
   (test-is/is 
    (true? (math/numeric? (long 1024))))
   (test-is/is 
    (true? (math/numeric? (bigdec 1024.4))))
   (test-is/is
    (true? (math/numeric? (double 1024.4))))
   (test-is/is
    (true? (math/numeric? (float 1024.4))))
   (test-is/is
    (true? (math/numeric? 3/4)))
   (test-is/is
    (false? (math/numeric? "hello")))
   (test-is/is
    (false? (math/numeric? 'a)))
   (test-is/is
    (false? (math/numeric? nil)))
  )

  
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
   (true? (= (.sin Math (/ math/+pi+ 2)) 1))))

(test-is/deftest test-e
  ;; 
  (test-is/is 
   (true? (= (.log Math math/+e+) 1))))


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
   (true? (= (math/log 10 10) 1)))
  (test-is/is 
   (true? (= (math/log 10 100) 2)))
  (test-is/is 
   (true? (= (math/log 2 1024) 10)))

  )


(test-is/deftest test-log10
  (test-is/is 
   (true? (= (math/log10 10) 1)))
  (test-is/is 
   (true? (= (math/log10 100) 2)))
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
   (true? (= (math/sqrt 4) 2)))
  )

(test-is/deftest test-root
  (test-is/is 
   (true? (= (math/root 3 27) 3)))
  (test-is/is 
   (true? (= (math/root 2 4) 2)))
  (test-is/is 
   (true? (= (float (math/root 3 (math/exp 3))) (float  math/+e+))))
  )

(test-is/deftest test-ceil
  (test-is/is 
   (true? (= (math/ceil 3.4) 4)))
  (test-is/is 
   (true? (= (math/ceil -1.2) -1)))

  )

(test-is/deftest test-floor
  (test-is/is 
   (true? (= (math/floor 3.4) 3)))
  (test-is/is 
   (true? (= (math/floor -1.2) -2)))
  )

(test-is/deftest test-factorial
  (test-is/is 
   (true? (= (math/factorial 5) 120)))
  (test-is/is 
   (true? (= (math/factorial 0) 1)))
  (test-is/throws IllegalArgumentException (math/factorial -1))
  )

(test-is/deftest test-to-degrees
  (test-is/is 
   (true? (= (math/to-degrees math/+pi+) 180)))
)

(test-is/deftest test-to-radians
  (test-is/is 
   (true? (= (math/to-radians 180) math/+pi+)))
  )


(test-is/deftest test-summation
  (test-is/is 
   (true? (= (math/summation i (range 10) i) (apply + (range 10))))
  ))
		 


(test-is/run-tests)


 
