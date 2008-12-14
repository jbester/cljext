(defn ols
  "(ols points)
Ordinary Least Squares

points - a list of datapoints in the form (x,y) e.g. [(1,2),(2,4),(4,8)]

Returns:
  (slope est, intercept est )
"
  ([points]
   (let [[x_points y_points] (unzip points)
	 len (count points)
	 sum_x (apply + x_points)
	 sum_y (apply + y_points)
	 ;; average x and y
	 mean_x (/ sum_x len)
	 mean_y (/ sum_y len)]
     (with-local-vars [sum_xx 0 
		       sum_xy 0
		       sum_yy 0]
       (do
	 ;; sums for each deviation from th emean
	 (doseq [[x y] points]
	   (let [x_dev (- x mean_x)
		 y_dev (- y mean_y)]
	     (var-set sum_xx (+ (var-get sum_xx) (* x_dev x_dev)))
	     (var-set sum_xy (+ (var-get sum_xy) (* x_dev y_dev)))
	     (var-set sum_yy (+ (var-get sum_yy) (* y_dev y_dev)))))
	 ;; calculate beta, alpha, and corr
	 (let [beta (/ (var-get sum_xy) (var-get sum_xx))
	       alpha (- mean_y (* beta mean_x))
	       corr (/ (* (var-get sum_xy) (var-get sum_xy)) (* (var-get sum_xx)  (var-get sum_yy)))]
	   [ beta, alpha]))))))

