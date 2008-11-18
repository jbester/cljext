(ns cljext.binpack.utest
    (:refer-clojure)
    (:require [cljext.binpack :as binpack]
	      [cljext.limits :as limits]
	      [clojure.contrib.test-is :as test-is])
    (:refer cljext.limits)
    )


(defn loopback
  ([fmt & params]
   (test-is/is 
    (= (binpack/unpack fmt (apply binpack/pack (cons fmt params))) params))))

(defn check-argument
  ([fmt & params]
   (test-is/throws IllegalArgumentException (apply binpack/pack (cons fmt params)))))

(test-is/deftest test-loopback
  ;; test padding
  (loopback ">x")
  (loopback ">xxx")
  (test-is/is 
   (= (binpack/unpack "<xxx" (binpack/pack "<xxx")) nil))
  (loopback "<x")
  ;; test signed byte
  (loopback "<b" 0)
  (loopback "<b" +min-byte+)
  (loopback "<b" +max-byte+)
  (loopback ">b" +min-byte+)
  (loopback ">b" +max-byte+)
  ;; signed byte mixed endian (one byte doesn't matter)
  (test-is/is 
   (= (binpack/unpack "<b" (binpack/pack ">b" -1)) (list -1)))
  ;; check arguments
  (check-argument "b" -129)
  (check-argument "b" 128)


  ;; test unsigned byte
  (loopback "<B" +min-unsigned-byte+)
  (loopback "<B" +max-unsigned-byte+)
  (loopback ">B" +min-unsigned-byte+)
  (loopback ">B" +max-unsigned-byte+)

  (check-argument "B" (dec +min-unsigned-byte+))
  (check-argument "B" (inc +max-unsigned-byte+))

  ;; test signed short
  (loopback ">h" +min-short+)
  (loopback ">h" +max-short+)
  (loopback "<h" +min-short+)
  (loopback "<h" +max-short+)
  (check-argument "h" (dec +min-short+))
  (check-argument "h" (inc +max-short+))

  ;; test unsigned short
  (loopback ">H" +min-unsigned-short+)
  (loopback ">H" +max-unsigned-short+)
  (loopback "<H" +min-unsigned-short+)
  (loopback "<H" +max-unsigned-short+)
  (check-argument "H" (dec +min-unsigned-short+))
  (check-argument "H" (inc +max-unsigned-short+))

  ;; test signed int
  (loopback ">i" +min-integer+)
  (loopback ">i" +max-integer+)
  (loopback "<i" +min-integer+)
  (loopback "<i" +max-integer+)
  (check-argument "i" (dec +min-integer+))
  (check-argument "i" (inc +max-integer+))

  ;; test unsigned int
  (loopback ">I" +min-unsigned-integer+)
  (loopback ">I" +max-unsigned-integer+)
  (loopback "<I" +min-unsigned-integer+)
  (loopback "<I" +max-unsigned-integer+)
  (check-argument "I" (dec +min-unsigned-integer+))
  (check-argument "I" (inc +max-unsigned-integer+))

  ;; test signed long
  (loopback ">l" +min-long+)
  (loopback ">l" +max-long+)
  (loopback "<l" +min-long+)
  (loopback "<l" +max-long+)
  (check-argument "l" (dec +min-long+))
  (check-argument "l" (inc +max-long+))

  ;; test unsigned lng
  (loopback ">L" +min-unsigned-long+)
  (loopback ">L" +max-unsigned-long+)
  (loopback "<L" +min-unsigned-long+)
  (loopback "<L" +max-unsigned-long+)
  (check-argument "L" (dec +min-unsigned-long+))
  (check-argument "L" (inc +max-unsigned-long+))


  )

(test-is/run-tests)
