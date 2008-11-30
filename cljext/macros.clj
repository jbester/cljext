(ns cljext.macros
  (:refer-clojure))

(defmacro switch
  "Switch similar to code e.g. (switch 'a 'b (print 'b) 'c (print 'c) :default (print 'default))"
  ([value & cases]
   (let [sym (gensym)]
     `(let [~sym ~value]
	(cond 
	 ~@(apply concat 
		  (for [case (partition 2 cases)]
		    (if (= (first case) :default)
		      `(true ~(second case))
		      `((= ~sym ~(first case)) ~(second case))))))))))


(defmacro with-syms
  "Generate symbols for a list of variables"
  ([syms & body]
     `(let ~(apply vector
		   (apply concat
			  (for [sym syms]
			    `(~sym (gensym)))))
	~@body)))
