

(require 'cljext.math)
(require 'cljext.seq)
(require 'cljext.macros)
(refer 'cljext.math)
(refer 'cljext.seq)
(refer 'cljext.macros)


(defn derivative 
  "Calculate the derivative"
  ;; return a function to calculate the derivative
  ([func]
   (fn [x & [h]]
       (derivative func x h)))
  ;; calculate the derivative value
  ([func x & [h]]
   (let [h (if (nil? h) 1e-8 h)]
     (/ (- (func (+ x h)) (func x)) h))))

(defn expand-interval
  "Expand an interval until points are on opposite sides of the root"
  ([func x1 x2 & [max-iterations]]
     (with-default-values [max-iterations 20]
      (with-local-vars [low (min x1 x2)
			hi (max x1 x2)]
	 (when (= @low @hi)
	   (throw 
	    (Exception. "Impossible range: upper and lower bounds are equal")))
	 (loop [i (range max-iterations)]
	   ;; calculate 
	   ;; a = f( low ) 
	   ;; b = f( hi )
	   (let [a (func @low)
		 b (func @hi)]
	     ;; no more iterations left
	     (when (empty? (rest i))
	       (throw (Exception. "Could not find bracketabe root.")))
	     ;; check if a * b is negative
	     ;; meaning one point is on the positive side of the root
	     ;; the other on the negative size of the root
	     (if (neg? (* a b))
	       [@low @hi] ;; return interval
	       (do
		 ;; move values approximately equally;
		 ;; therefore move the one that is closest to zero
		 ;; away from zero
		 (if (< (abs a) (abs b))
		   (var-set low (+ @low (- @low @hi)))
		   (var-set hi (+ @hi (- @hi @low))))
		 (recur (rest i))))))))))


(defn contract-interval
  "Contract an interval while points are on opposite sides of the root"
  ([func x1 x2 & [segments max-iterations]]
     (with-default-values [segments 10
			   max-iterations 30]
       (let [low (min x1 x2)
	     hi (max x1 x2)]
	 (when (= low hi)
	   (throw (Exception. "Impossible range; uppwer and lower bounds are equal")))
	 (when (pos? (* low hi))
	   (throw (Exception. "Not a vlid interval")))
	 (loop [i (range max-iterations)
		low low
		hi hi]
	   ;; move both numbers equally
	   (let [a (func low)
		 b (func hi)
		 ;; new candidate for low
		 low-prime (if (< a b)
			     (- low (/ (- low hi) segments))
			     low)
		 ;; new candidate for hi
		 hi-prime (if (< a b)
			    hi
			    (- hi (/ (- hi low) segments)))]
	     ;; shadow a and b with the new candidates
	     (let [a (func low-prime)
		   b (func hi-prime)]
	       ;; if last iteration or 
	       (if (or (pos? (* a b)) (nil? (rest i)))
		 [low hi]
		 (recur (rest i) low-prime hi-prime)))))))))


(defn find-interval
  "Find an interval based on initial guesses"
  ([func x1 x2]
     (let [[low hi] (expand-interval func x1 x2)]
       (contract-interval func low hi))))

(defn bisect
  "Find root by bisection method"
  ([func x1 x2 & [max-iterations precision]]
     (with-default-values [max-iterations 20
			   precision 1e-3]
     ;; initial values
     (let [low (min x1 x2)
	   hi (max x1 x2)
	   a (func low)
	   b (func hi)]
       ;; check if it's possible
       (when (pos? (* a b))
	 (throw (Exception. "Root is not bracketed within interval")))
       ;; loop i times
       (loop [i (range max-iterations) 
	      low low
	      hi hi]
	 ;; calculate values for a b & c
	 (let [a (func low)
	       b (func hi)
	       midvalue (+ low (* 0.5 (- hi low)))
	       c (func midvalue)]

	   (cond (neg? (* a c))  ;; if a & c are different sides of the root
		 ;; move swap hi with the midpoint
		 (recur (rest i) low midvalue)
		 (neg? (* b c)) ;; if b  & c are different sides of the root...
		 (recur (rest i) midvalue hi)
		 (zero? (* a c)) ;; if zero determine which pointer is the zero
		 (if (zero? a)
		   low
		   midvalue)
		 ;; if midpoint is within tolerqance or hit the max
		 ;; on iterations return the midvalue
		 (or (<= (abs c) precision) (nil? (rest i)))
		 midvalue)))))))

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
