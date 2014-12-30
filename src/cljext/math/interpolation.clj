(require 'cljext.math)
(require 'cljext.seq)
(require 'cljext.macros)
(refer 'cljext.math)
(refer 'cljext.seq)
(refer 'cljext.macros)

(defn linear-interpolation 
  "Return a function that interpolates for a set of points"
  ([points]
     (let [[x-points y-points] (unzip points)
	   x-points (list->vector x-points)
	   y-points (list->vector y-points)]
       (fn [point]
	 (with-local-vars [result nil
			   start nil
			   end nil]
	   ;; loop through all points and try to find
	   ;; either the exact point or the start and end
	   ;; of the line segment the point exists on
	   (doseq [i (range (count points))]
	     :while (and (nil? @end)
			 (nil? @result))
	     (let [x (nth x-points i)]
	       (cond (= x point)
		     (var-set result (nth y-points i))
		     (> x point)
		     (var-set start i)
		     (< x point)
		     (var-set end i))))
	   ;; if the point was found return it
	   (cond (not (nil? @result))
		 @result
		 ;; if one part of the line segment was found
		 (or (nil? @start) (nil? @end))
		 (throw (Exception. "Cannot extrapolate data"))
		 true
		 ;; interpolate data
		 (let [num (- (nth y-points @end) 
			      (nth y-points @start))
		       den (- (nth x-points @end)
			      (nth x-points @start))
		       ;; calculate slope
		       slope (/ num den)
		       ;; calculate how far down the slope to apply
		       delta (- point (nth x-points @start))]
		   ;; calculate the interpolated point
		   (+ (nth y-points @start) (* delta slope)))))))))
		     
(defn legrange-basis-polynomial
  "Return the jth legrange basis polynomial for a given set of points
PRODUCT(i=0..k when i!=j,(x - x[i])/(x[j] - x[i]))
"
  ([points j]
     (fn [x]
       (product [i (range (count points))]
		(if (= i j)
		  1
		  (let [x_i (nth points i)
			x_j (nth points j)]
		    (formula ( x - x_i ) / ( x_j - x_i ))))))))

(defn legrange-interpolation 
  ([points]
     (let [points (list->vector points)
	   ;; get the x y points seperated as vectors
	   [x-points y-points] (map list->vector (unzip points))
	   ;; create a basis function generator
	   basis-gen (partial legrange-basis-polynomial x-points)
	   ;; create basis functions from 0 through (count points)
	   basis-fns (vector-tabulate (count points) basis-gen)]
       (fn [x]
	 (with-local-vars [result 0] 
	   (dotimes [i (count points)]
	     (let [func (nth basis-fns i)]
	       ;; summation( i := 0 .. #basis, y[i] * basis_i x[i] )
	       (var-set result 
			(+ @result (* (nth y-points i) (func x))))))
	   @result)))))

	     
	   

       