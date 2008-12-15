(require 'cljext.seq)
(require 'cljext.macros)
(refer 'cljext.seq)
(refer 'cljext.macros)

(def +valid-for+ {:for :from :to :in})

(defn parse-clauses
  ([clauses +valid-keywords+]
     (loop [c clauses
	    result nil]
       (if (empty? c)
	 [result lst]
	 (let [[keyword expr :as clause] c
	     lst (rest c)]
	   (if (not (contains? +valid-keywords+ keyword)))
	   [result lst]
	   (recur lst (cons clause result))))))

(defn parse-loop
  ([clauses]
     (loop [groups [] clauses clauses]
       (do 
	 (if (nil? clauses)
	   groups
	   (let [[keyword expr :as clause] (first clauses)]
	     (cond (= keyword :for)
		   (let [[rst result] (parse-clauses clauses +valid-for+)]
		     (do 
		       (println [result rst])
		       (recur (conj groups result) rst))))))))))
;; (defmacro loop-macro
;;   ([& clauses]
;;      (let [clauses (partition 2 clauses)]
     