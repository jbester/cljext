(require 'cljext.math)
(require 'cljext.seq)
(require 'cljext.macros)
(refer 'cljext.math)
(refer 'cljext.seq)
(refer 'cljext.macros)

(defn list->vector
  ([col]
     (apply vector col)))


(defn linear-interpolation 
  ([points]
     (let [[x-points y-points] (unzip points)
	   x-points (list->vector x-points)
	   y-points (list->vector y-points)]
       (fn [point]
	 (with-local-vars [result nil
			   start nil
			   end nil]
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
	   (cond (not (nil? @result))
		 result
		 (or (nil? @start) (nil? @end))
		 (throw (Exception. "Cannot extrapolate data"))
		 true
		 (let [num (- (nth y-points @end) 
			      (nth y-points @start))
		       den (- (nth x-points @end)
			      (nth x-points @start))
		       slope (/ num den)
		       delta (- point (nth x-points @start))]
		   (+ (nth y-points @start) (* delta slope)))))))))
		     
		 
	     
	   
       