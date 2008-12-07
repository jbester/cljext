(import '(Jama.Matrix))

(defmacro with-syms
  ([syms & body]
     `(let ~(apply vector
		   (apply concat
			  (for [sym syms]
			    `(~sym (gensym)))))
	~@body)))

(defn rows
  ([matrix]
     (.getRowDimension matrix)))

(defn columns
  ([matrix]
     (.getColumnDimension matrix)))

(defmacro iterate-data
  ([matrix i j cell & body]
     (with-syms
	 [row-count col-count xrange yrange rows cols row mat]
       `(let [~mat ~matrix
	      ~row-count (count ~mat)
	      ~col-count (count (first ~mat))]
	  (loop [~xrange (range ~row-count)
		 ~rows ~mat]
	    (if (nil? ~xrange)
	      nil
	      (let [~row (first ~rows)
		    ~i (first ~xrange)]
		(loop [~yrange (range ~col-count)
		       ~cols ~row]
		  (if (nil? ~yrange)
		    nil
		    (let [~j (first ~yrange)
			  ~cell (first ~cols)]
		      ~@body
		      (recur (rest ~yrange) (rest ~cols)))))
		(recur (rest ~xrange) (rest ~rows)))))))))

(defn mget
  ([matrix i j]
     (.get matrix i j)))

(defn mset
  ([matrix i j val]
     (.set matrix i j val)))

(defn arange
  ([& rest]
     (into-array Integer/TYPE (apply range rest ))))

 (defn slice
   ([array stop]
      (slice array 0 stop 1))
   ([array start stop]
      (slice array start stop 1))
   ([array start stop step]
      (let [len (alength array)
	    stop (if (>= stop 0) stop (+ len stop))
	    start (if (>= start 0) start (+ len start))]
	(loop [range (range start stop step)
	       result []]
	  (let [pos (first range)]
	    (if (empty? range)
	      result
	      (recur (rest range) (conj result (aget array pos)))))))))

(defn make-matrix
  ([values]
     (let [rows (count values)
	   cols (count (first values))
	   array (make-array Double/TYPE rows cols)]
       (iterate-data values i j cell
	 (aset array i j cell))
       (Jama.Matrix/constructWithCopy array))))
     
(defn make-vector
  ([vect]
     (make-matrix (map vector vector))))



(defn print-matrix
  ([matrix]
     (doseq [i (range (rows matrix))]
       (doseq [j (range (columns matrix))]
	 (let [cell (.get matrix i j)]
	   (cond (and (> i 0) (= j 0))
		 (do (println "]") (print "[ " cell " "))
		 (and (= i 0) (= j 0))
		 (print "[ " cell " ")
		 true
		 (print cell " ")))))
     (println "]")))

(defn transpose
  ([matrix]
     (.transpose matrix)))



	 