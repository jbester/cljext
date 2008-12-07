(require 'cljext.math)
(require 'cljext.seq)
(require 'cljext.macros)
(refer 'cljext.math)
(refer 'cljext.seq)
(refer 'cljext.macros)

(defn expand-interval
  ([func x1 x2 & [max-iterations]]
     (with-default-values [max-iterations 20]
       (with-local-vars [a 0
			 b 0
			 low (min x1 x2)
			 hi (max x1 x2)]
	 (when (= @low @hi)
	   (throw 
	    (Exception. "Impossible range: upper and lower bounds are equal")))
	 (loop [i (range max-iterations)]
	   ;; calculate 
	   ;; a = f( low ) 
	   ;; b = f( hi )
	   (var-set a (func @low))
	   (var-set b (func @hi))
	   ;; no more iterations left
	   (when (empty? (rest i))
	     (throw (Exception. "Could not find bracketabe root.")))
	   ;; check if a * b is negative
	   ;; meaning one point is on the positive side of the root
	   ;; the other on the negative size of the root
	   (if (neg? (* @a @b))
	     [low hi] ;; return interval
	     (do
	       ;; move values approximately equally;
	       ;; therefore move the one that is closest to zero
	       ;; away from zero
	       (if (< (abs @a) (abs @b))
		 (var-set low (+ @low (- @low @hi)))
		 (var-set hi (+ @hi (- @hi @low))))
	       (recur (rest i)))))))))
  
  

(defn newton-raphson
  "Calculate a root using the newton-raphson method

func - function to take the root of
x-initial - initial guess to seed
max-iterations - maximum number of iterations to perform
precision - distance from zero within which a possible solution is considered valid

Returns:
root
"
  ([func x-initial & [max-iterations precision]]
     ;; handle default values for optional parameters
     (with-default-values [max-iterations 10
			   precision 1e-3]
       ;; get the derivative
       (let [dfunc (derivative func)]
	 ;; set up the local vars
	 (with-local-vars [midvalue 0
			   x x-initial]
	   ;; loop using the method of 
	   ;; x[n+1] = x[n] - f( x[n] )/f'(x[n])
	   (loop [rng (range max-iterations)]
	     (let [fx (func @x)
		   dfx (dfunc @x)]
	      (when (zero? dfx)
		(throw (Exception. "Local minimum/maximum found")))
	      (var-set x (- @x (/ fx dfx)))
	      ;; check if candidate is within precision
	      (let [candidate (func @x)]
		(if (or (<= (abs candidate) precision) (empty? (rest rng)))
		  @x
		  (recur (rest rng)))))))))))
