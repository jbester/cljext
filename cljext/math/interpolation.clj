(require 'cljext.math)
(require 'cljext.seq)
(require 'cljext.macros)
(refer 'cljext.math)
(refer 'cljext.seq)
(refer 'cljext.macros)

(defn list->vector
  "Convert a list to a vector"
  ([col]
     (apply vector col)))


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
		     
		 
	     
	   
       