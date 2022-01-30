;;; A tiny benchmark of integer set implementations for three three things:
;;; 1. Populating an integer set with mostly clustered values. 
;;;    In this test there is a gap approximately every 5th element.
;;; 2. Checking for whether the integer set contains values.
;;; 3. Doing an ordered traversal of the values in the set.

;;; Assumes you have loaded the following into your lisp:
;;; cl-roaring (https://github.com/dtenny/cl-roaring)
;;; clustered-intset *FINISH*
;;; cl-intset (https://github.com/tkych/cl-intset)
;;; fset (via quicklisp)

;;; In the case of the result numbers appended below, all libraries were loaded
;;; with SPEED 3 SAFETY 0 in effect, except possibly for FSET on which I did a
;;; `(asdf:compile-system :fset :force t)` in my declaimed environment, hopefully that was 
;;; sufficient.

;;; Using 1,000,000 intset entries, 20% holes, ranging from 1,000,000 to 2,250,239
;;; Duration in seconds, bytes consed (Kilobytes/Megabytes/Gigabytes etc).
;;; Lower numbers are better.
;;;
;;;                                                    Ascending
;;; Implementation     Loading           Membership    Traversal
;;; =================  ===============   ===========   =============
;;; Simple Bit Vector   0.005s,  ~281K   0.003s,  0K   0.008s,    0K 
;;; Hash Table          0.032s,   ~24M   0.032s,  0K   0.299s,   ~8M
;;; Clustered Intset    0.046s,  ~490K   0.030s,  0K   0.022s, ~161K
;;; CL-Roaring (FFI)    0.014s,    N/A   0.024s, N/A   0.011s,   N/A
;;; FSET                1.592s,  ~1.5G   0.429s,  0K   0.006s,    0K  
;;; CL-intset          50.102s,  ~406G   0.016s,  0K   0.020s,    0K
;;;
;;; Using 1,000,000 intset entries, 20% holes, ranging from 1,000,000,000 to 1,001,248,861
;;;
;;;                                                    Ascending
;;; Implementation     Loading           Membership    Traversal
;;; =================  ===============   ===========   =============
;;; Simple Bit Vector   0.010s,  ~125M   0.004s,  0K   2.542s,    0K 
;;; Hash Table          0.032s,   ~24M   0.031s,  0K   0.297s,   ~8M
;;; Clustered Intset    0.048s,  ~490K   0.029s,  0K   0.022s, ~161K
;;; CL-Roaring (FFI)    0.013s,    N/A   0.023s, N/A   0.010s,   N/A
;;; FSET                1.535s,  ~1.5G   0.403s,  0K   0.005s,    0K  
;;; CL-intset              <cancelled after a couple of hours>


;;; Interpretation:
;;; 0  The simple bit vector, hash table, and clustered intset memory values on allocation
;;;    pretty much reflect the size of the data structure that persists after population.
;;;    This is not true for FSET, where most of consed bytes are probably due to my poor
;;;    coding of populating set. I don't know what the actual (sticky) memory usage is.
;;; 1. CL simple bit vectors are terrific if you can use them. If your int-set members
;;;    are > 10^6 though you need to start thinking hard about them, depending on usage.
;;; 2. The CL-ROARING FFI wrappers for the C library namesake is next up for speed.
;;;    Its memory should be good but it isn't in the lisp memory and `time`/gc won't see it.
;;;    You're limited to the range of unit32 with cl-roaring, which may be to small for some 
;;;    applications. On the other hand, if you have many, but less clustered keys, this is
;;;    a good option if the values are in the uint32 range and are willing to have the
;;;    external library dependency.
;;; 3. The CLUSTERED-INTSET behavior is a decent enough compromise with good memory usage through
;;;    fixnums-as-bitsets,  O(logN) hash table lookups, and a two-order-of-magnitude reduction
;;;    of intset members as hash-table keys. It can be slightly more complutationally expensive
;;;    than plain hash tables, but uses substantially less memory for clustered keys:
;;;    490KB in this test of 1M keys, vs 24MB+ for a plain hash-table with faster ordered
;;;    traversal because of a substantially reduced key set (so a smaller sort operation).
;;; 4. The 'bignums' as bit fields approach of CL-INTSET.  Don't go there for large valued
;;;    integers. More an academic thought exercise than a serious candidate for integer set
;;;    representations (with apologies to the author(s)). There are multiple packages out
;;;    there that take this approach, I must be missing the point.
;;; 5. FSET: slow to populate, is there a faster way?  Reasonable to search, Fast to iterate
;;;    in apparently sorted fashion.  Unclear if the 1.5GB consed on creation was because
;;;    I was doing it wrong, or because of its immutable semantics. 

(defun my-gc ()
  (let ((f (pop sb-ext:*after-gc-hooks*)))
    (loop repeat 3 do (sb-ext:gc :full t))
    (when f
      (push f sb-ext:*after-gc-hooks*)))
  #+nil(room))

(defvar *test-vals* (make-array 1 :element-type '(integer 0 *) :initial-element 0))
(defvar *n* 0 "Number of entries in *test-vals*")
(defvar *start* 0 "First entry in *test-vals*")

(defun make-test-data (n start)
  "Make integer set test data with N values."
  (setf *test-vals* (make-array n :element-type '(integer 0 *) :initial-element 0))
  (setf *n* n)
  (setf *start* start)
  (loop for i from start
        with idx = 0
        while (< idx n)
        unless (= 0 (random 5))         ;20% holes in our membership
          do (setf (svref *test-vals* idx) i 
                   idx (1+ idx))
        finally (format t "*** Using ~:d intset entries, 20% holes, ranging from ~:d to ~:d~%"
                        n start i))
  ;;(format t "*** Memory before test~%")
  (my-gc))

(make-test-data 1000000 1000000)        ;1M starting at 1 million
(make-test-data 1000000 1000000000)     ;1M starting at 1 billion

(defun ci-test ()
  (format t "~%*** CLUSTERED-INTSET TEST ***~%")
  (my-gc)
  (push #'(lambda () (print "GC")) sb-ext:*after-gc-hooks*)
  (unwind-protect
       (progn
         (format t "~%*** Load iset from previously created ~d test values~%" *n*)
         (let ((iset (time (clustered-intset:seq->intset *test-vals*))))
           (format t "*** Loaded hashtable has ~:d entries."
                   (hash-table-count (clustered-intset::intset-ht iset)))
           (my-gc)
           ;;(dump-ht (intset-ht iset))
           (format t "~%*** contains-p~%")
           (time (loop for i from *start*
                       with count = 0
                       while (< count *n*)
                       do (when (clustered-intset:contains-p i iset)
                            (incf count))))
           (my-gc)

           (format t "~%*** first/next (KNOWN BAD - SHOULD PROBABLY DELETE THESE)~%")
           (time (loop with count = 1
                       with current = (clustered-intset:first iset)
                       as   next = (clustered-intset:next current iset)
                       while next
                       do   (setq current next count (1+ count))
                       finally (unless (= count *n*)
                                 (format t "~%**** ERROR: ~:d != ~:d ****~%"
                                         count *n*))))
           (my-gc)

           (format t "~%*** iterator/advance~%")
           (time (loop with iterator = (clustered-intset:iterator iset)
                       with count = 0
                       as   next = (clustered-intset:advance iterator)
                       while next
                       do   (incf count)
                       finally (unless (= count *n*)
                                 (format t "~%**** ERROR: ~:d != ~:d ****~%"
                                         count *n*))))
           (my-gc)

           ))
    (pop sb-ext:*after-gc-hooks*)))
      

(defun roaring-test ()
  (format t "~%*** CL-ROARING TEST ***~%")
  (my-gc)
  (push #'(lambda () (print "GC")) sb-ext:*after-gc-hooks*)
  (unwind-protect
       (progn
         (format t "~%*** Load iset from previously created ~d test values~%" *n*)
         (let ((iset (time (cl-roaring:create :initial-contents *test-vals*))))
           (format t "*** Loaded roaring-bitmap has ~:d entries."
                   (cl-roaring:cardinality iset))
           (my-gc)

           (format t "~%*** containsp~%")
           (time (loop for i from *start*
                       with count = 0
                       while (< count *n*)
                       do (when (cl-roaring:containsp i iset)
                            (incf count))))
           (my-gc)

           (format t "~%*** map-bitmap via cl-roaring iterator")
           (time (let ((count 0))
                   (cl-roaring:map-bitmap iset #'(lambda (e) (declare (ignore e)) (incf count)))
                   (unless (= count *n*)
                     (format t "~%**** ERROR: ~:d != ~:d ****~%"
                             count *n*))))
           (my-gc)

           (cl-roaring:free iset)))
    (pop sb-ext:*after-gc-hooks*)))

(defun numeric-intset-test ()
  (format t "~%*** CL-INTSET (numbers as int sets) TEST ***~%")
  (my-gc)
  ;;  (push #'(lambda () (print "GC")) sb-ext:*after-gc-hooks*) ; too much I/O for this test...
  (unwind-protect
       (progn
         (let ((listified-test-vals (coerce *test-vals* 'list)))
           (my-gc)
           (format t "~%*** Load iset from previously created ~d test values~%" *n*)
           (let ((iset (time (cl-intset:list->intset listified-test-vals))))
             (format t "*** Loaded roaring-bitmap has ~:d entries."
                     (cl-intset:size iset))
             (setf listified-test-vals nil)
             (my-gc)
           

             (format t "~%*** containsp~%")
             (time (loop for i from *start*
                         with count = 0
                         while (< count *n*)
                         do (when (cl-intset:memberp i iset)
                              (incf count))))
             (my-gc)

             (format t "~%*** map-bitmap via cl-intset for-each (no iterator)")
             (time (let ((count 0))
                     (cl-intset:for-each #'(lambda (e) (declare (ignore e)) (incf count)) iset)
                     (unless (= count *n*)
                       (format t "~%**** ERROR: ~:d != ~:d ****~%"
                               count *n*))))
             (my-gc))))
    (pop sb-ext:*after-gc-hooks*)))

(defun fset-test ()
  ;; I haven't used fset before, my code here may be rotten
  ;; https://fset.common-lisp.dev/Site/FSet-Tutorial.html
  (format t "~%*** FSET TEST ***~%")
  (my-gc)
  (push #'(lambda () (print "GC")) sb-ext:*after-gc-hooks*)
  (unwind-protect
       (progn
         (format t "~%*** Load iset from previously created ~d test values~%" *n*)
         (let ((iset (time 
                      #+nil ; no luck here, doesn't like the vector of input
                      (fset:set (fset:$ *test-vals*)) ;special syntax to use all values in another seq?
                      (let ((fs (fset:empty-set)))
                        (loop for elt across (reverse *test-vals*)
                              do (setq fs (fset:with fs elt)))
                        fs)

                      #+nil
                      (let ((fs (fset:empty-set)))
                        (loop for elt across *test-vals*
                              do (setq fs (fset:insert fs elt)))
                        fs))))
           (format t "*** Loaded fset has ~:d entries."
                   (fset:size iset))
           (my-gc)
           

           (format t "~%*** containsp~%")
           (time (loop for i from *start*
                       with count = 0
                       while (< count *n*)
                       do (when (fset:member? i iset) ; using function, vs generic contains?
                            (incf count))))
           (my-gc)

           (format t "~%*** ordered iteration with do-set ~%")
           (time (let ((count 0)
                       ;; I have verified the do-set order was sorted, and
                       ;; that it was sorted even when the input was reversed
                       #+nil (result (make-array *n* :fill-pointer 0)))
                   (fset:do-set (elt iset)
                     (declare (ignore elt))
                     #+nil (vector-push elt result)
                     (incf count))
                   #+nil
                   (unless (equalp result *test-vals*)
                     (format t "~%**** ITERATION WAS NOT SORTED ****~%"))
                   (unless (= count *n*)
                     (format t "~%**** ERROR: ~:d != ~:d ****~%"
                             count *n*))))
           (my-gc)))
    (pop sb-ext:*after-gc-hooks*)))

(defun bit-vector-test ()
  (format t "~%*** SIMPLE-BIT-VECTOR TEST ***~%")
  (my-gc)
  (push #'(lambda () (print "GC")) sb-ext:*after-gc-hooks*)
  ;; Fair? not fair?  no layer of abstraction here. No automatic growth either.
  ;; Brute forced, not tested, may have one-off bugs, etc.  Good enough for gov't work.
  ;; If course if our starting value was 1 billion instead of 1million we'd need 
  ;; 125MB instead of 125KB
  (unwind-protect
       (progn
         (my-gc)
         (format t "~%*** Load iset from previously created ~d test values~%" *n*)
         (let ((iset (time 
                      (let ((bv (make-array (1+ (elt *test-vals* (1- *n*))) :element-type 'bit)))
                        (declare (simple-bit-vector bv))
                        (loop for elt across *test-vals*
                              do (setf (sbit bv elt) 1))
                        bv))))
           (declare (simple-bit-vector iset))
           (format t "*** Loaded bit-vector has ~:d entries."
                   (length iset))
           (my-gc)

           (format t "~%*** containsp~%")
           (time (loop for i from *start*
                       with count = 0
                       while (< count *n*)
                       do (when (sbit iset i)
                            (incf count))))
           (my-gc)

           (format t "~%*** straight bit-vector iteration ")
           (time (let ((count 0))
                   ;; Gonna look at a lot of zero bits without tricks, but that's
                   ;; life with simple bit vectors.
                   (loop for elt across iset
                         when (= elt 1)
                         do (incf count))
                   (unless (= count *n*)
                     (format t "~%**** ERROR: ~:d != ~:d ****~%"
                             count *n*))))
           (my-gc)))
    (pop sb-ext:*after-gc-hooks*)))

(defun ht-test ()
  (format t "~%*** HASH-TABLE TEST ***~%")
  (my-gc)
  (push #'(lambda () (print "GC")) sb-ext:*after-gc-hooks*)
  (unwind-protect
       (progn
         (my-gc)
         (format t "~%*** Load iset from previously created ~d test values~%" *n*)
         (let ((iset (time 
                      (let ((ht (make-hash-table :size *n*)))
                        (declare (hash-table ht))
                        (loop for elt across *test-vals*
                              do (setf (gethash elt ht) elt))
                        ht))))
           (declare (hash-table iset))
           (format t "*** Loaded hash-table has ~:d entries."
                   (hash-table-count iset))
           (my-gc)

           (format t "~%*** containsp~%")
           (time (loop for i from *start*
                       with count = 0
                       while (< count *n*)
                       do (when (gethash i iset)
                            (incf count))))
           (my-gc)

           (format t "~%*** hash-table iteration, sorted for fairness")
           (time (let ((count (hash-table-count iset))
                       (result (make-array (hash-table-count iset) :fill-pointer 0)))
                   (loop for elt being the hash-keys of iset
                         do (vector-push elt result))
                   (sort result #'<)
                   ;; Use result so `sort` isn't eliminated as dead code.
                   (unless (= (elt result 0) (elt *test-vals* 0))
                     (format t "~%**** ERROR: ~:d != ~:d ****~%"
                             (elt result 0) (elt *test-vals* 0)))
                   (unless (= count *n*)
                     (format t "~%**** ERROR: ~:d != ~:d ****~%"
                             count *n*))))
           (my-gc)))
    (pop sb-ext:*after-gc-hooks*)))

(setf sb-ext:*after-gc-hooks* nil)
(dribble "iset-test.out")
(bit-vector-test)
(ht-test)
(ci-test)
(roaring-test)
(numeric-intset-test)
(fset-test)
(dribble)
(setf sb-ext:*after-gc-hooks* nil)

#|
(slightly edited) output (blank lines rearranged, GC notices counted where applicable).
Of course the roaring memory impact is largely outside of lisp's managed memory
so not well reported here.

Using 1,000,000 intset entries, 20% holes, ranging from 1,000,000 to 2,250,239

*** SIMPLE-BIT-VECTOR TEST ***

*** Load iset from previously created 1000000 test values
Evaluation took:
  0.005 seconds of real time
  0.005016 seconds of total run time (0.005016 user, 0.000000 system)
  100.00% CPU
  14,103,622 processor cycles
  281,360 bytes consed
  
*** Loaded bit-vector has 2,250,668 entries.
*** containsp
Evaluation took:
  0.003 seconds of real time
  0.003546 seconds of total run time (0.003502 user, 0.000044 system)
  133.33% CPU
  9,971,779 processor cycles
  0 bytes consed

*** straight bit-vector iteration 
Evaluation took:
  0.008 seconds of real time
  0.008121 seconds of total run time (0.008121 user, 0.000000 system)
  100.00% CPU
  22,827,890 processor cycles
  0 bytes consed
  

*** HASH-TABLE TEST ***

*** Load iset from previously created 1000000 test values
Evaluation took:
  0.032 seconds of real time
  0.031883 seconds of total run time (0.025829 user, 0.006054 system)
  100.00% CPU
  89,676,147 processor cycles
  24,194,400 bytes consed
  
*** Loaded hash-table has 1,000,000 entries.
*** containsp
Evaluation took:
  0.032 seconds of real time
  0.031627 seconds of total run time (0.031627 user, 0.000000 system)
  100.00% CPU
  88,935,127 processor cycles
  0 bytes consed
  
*** hash-table iteration, sorted for fairness
Evaluation took:
  0.299 seconds of real time
  0.298478 seconds of total run time (0.298126 user, 0.000352 system)
  99.67% CPU
  838,338,053 processor cycles
  8,000,016 bytes consed


*** CLUSTERED-INTSET TEST ***

*** Load iset from previously created 1000000 test values
Evaluation took:
  0.046 seconds of real time
  0.046002 seconds of total run time (0.046002 user, 0.000000 system)
  100.00% CPU
  129,331,538 processor cycles
  489,936 bytes consed
  
*** Loaded hashtable has 20,173 entries.
*** contains-p
Evaluation took:
  0.030 seconds of real time
  0.029878 seconds of total run time (0.029878 user, 0.000000 system)
  100.00% CPU
  84,010,836 processor cycles
  0 bytes consed

*** first/next (KNOWN BAD - SHOULD PROBABLY DELETE THESE)
Evaluation took:
  1.446 seconds of real time
  1.443793 seconds of total run time (1.443793 user, 0.000000 system)
  99.86% CPU
  4,059,424,645 processor cycles
  0 bytes consed
  
*** iterator/advance
Evaluation took:
  0.022 seconds of real time
  0.022058 seconds of total run time (0.022058 user, 0.000000 system)
  100.00% CPU
  61,996,755 processor cycles
  161,408 bytes consed


*** CL-ROARING TEST ***

*** Load iset from previously created 1000000 test values
Evaluation took:
  0.014 seconds of real time
  0.013398 seconds of total run time (0.012459 user, 0.000939 system)
  92.86% CPU
  37,687,003 processor cycles
  0 bytes consed
  
*** Loaded roaring-bitmap has 1,000,000 entries.
*** containsp
Evaluation took:
  0.024 seconds of real time
  0.023385 seconds of total run time (0.023385 user, 0.000000 system)
  95.83% CPU
  65,731,643 processor cycles
  0 bytes consed

*** map-bitmap via cl-roaring iterator
Evaluation took:
  0.011 seconds of real time
  0.010638 seconds of total run time (0.010638 user, 0.000000 system)
  100.00% CPU
  29,915,131 processor cycles
  0 bytes consed


*** CL-INTSET (numbers as int sets) TEST ***

*** Load iset from previously created 1000000 test values
Evaluation took:
  50.102 seconds of real time
  52.314501 seconds of total run time (47.446904 user, 4.867597 system)
  [ Run times consist of 13.889 seconds GC time, and 38.426 seconds non-GC time. ]
  104.42% CPU
  140,686,425,197 processor cycles
  406,344,076,560 bytes consed
  
*** Loaded roaring-bitmap has 1,000,000 entries.
*** containsp
Evaluation took:
  0.016 seconds of real time
  0.016018 seconds of total run time (0.016018 user, 0.000000 system)
  100.00% CPU
  45,015,525 processor cycles
  0 bytes consed
  
*** map-bitmap via cl-intset for-each (no iterator)
Evaluation took:
  0.020 seconds of real time
  0.019956 seconds of total run time (0.019956 user, 0.000000 system)
  100.00% CPU
  56,093,415 processor cycles
  0 bytes consed


*** FSET TEST ***

*** Load iset from previously created 1000000 test values

<15 GC notices removed>
Evaluation took:
  1.592 seconds of real time
  1.600090 seconds of total run time (1.541555 user, 0.058535 system)
  [ Run times consist of 0.158 seconds GC time, and 1.443 seconds non-GC time. ]
  100.50% CPU
  4,472,310,577 processor cycles
  1,519,755,552 bytes consed
  
*** Loaded fset has 1,000,000 entries.
*** containsp
Evaluation took:
  0.429 seconds of real time
  0.428783 seconds of total run time (0.428429 user, 0.000354 system)
  100.00% CPU
  1,205,431,737 processor cycles
  0 bytes consed

*** ordered iteration with do-set 
Evaluation took:
  0.006 seconds of real time
  0.005684 seconds of total run time (0.005684 user, 0.000000 system)
  100.00% CPU
  16,000,454 processor cycles
  0 bytes consed
  
------------------------------------------------------------------------------

Using 1,000,000 intset entries, 20% holes, ranging from 1,000,000,000 to 1,001,248,861

*** SIMPLE-BIT-VECTOR TEST ***

*** Load iset from previously created 1000000 test values

Evaluation took:
  0.010 seconds of real time
  0.009972 seconds of total run time (0.005268 user, 0.004704 system)
  [ Run times consist of 0.003 seconds GC time, and 0.007 seconds non-GC time. ]
  100.00% CPU
  26,070,214 processor cycles
  125,156,320 bytes consed
  
*** Loaded bit-vector has 1,001,248,861 entries.
*** containsp
Evaluation took:
  0.004 seconds of real time
  0.003930 seconds of total run time (0.003930 user, 0.000000 system)
  100.00% CPU
  11,050,602 processor cycles
  0 bytes consed

*** straight bit-vector iteration 
Evaluation took:
  2.542 seconds of real time
  2.537549 seconds of total run time (2.518205 user, 0.019344 system)
  99.84% CPU
  7,138,445,642 processor cycles
  0 bytes consed


*** HASH-TABLE TEST ***

*** Load iset from previously created 1000000 test values
Evaluation took:
  0.032 seconds of real time
  0.031945 seconds of total run time (0.023886 user, 0.008059 system)
  100.00% CPU
  89,892,772 processor cycles
  24,194,400 bytes consed
  
*** Loaded bit-vector has 1,000,000 entries.
*** containsp
Evaluation took:
  0.031 seconds of real time
  0.030961 seconds of total run time (0.030961 user, 0.000000 system)
  100.00% CPU
  87,047,411 processor cycles
  0 bytes consed
  
*** hash-table iteration, sorted for fairness
Evaluation took:
  0.297 seconds of real time
  0.296293 seconds of total run time (0.293668 user, 0.002625 system)
  99.66% CPU
  832,349,244 processor cycles
  8,000,016 bytes consed


*** CLUSTERED-INTSET TEST ***

*** Load iset from previously created 1000000 test values
Evaluation took:
  0.048 seconds of real time
  0.048777 seconds of total run time (0.048773 user, 0.000004 system)
  102.08% CPU
  137,185,946 processor cycles
  489,424 bytes consed
  
*** Loaded hashtable has 20,144 entries.
*** contains-p
Evaluation took:
  0.029 seconds of real time
  0.029614 seconds of total run time (0.029602 user, 0.000012 system)
  103.45% CPU
  83,235,966 processor cycles
  0 bytes consed
  
*** first/next (KNOWN BAD - SHOULD PROBABLY DELETE THESE)
Evaluation took:
  1.429 seconds of real time
  1.427977 seconds of total run time (1.427977 user, 0.000000 system)
  99.93% CPU
  4,013,840,921 processor cycles
  0 bytes consed
  
*** iterator/advance
Evaluation took:
  0.022 seconds of real time
  0.021577 seconds of total run time (0.021577 user, 0.000000 system)
  100.00% CPU
  60,646,885 processor cycles
  161,168 bytes consed


*** CL-ROARING TEST ***

*** Load iset from previously created 1000000 test values
Evaluation took:
  0.013 seconds of real time
  0.013302 seconds of total run time (0.013294 user, 0.000008 system)
  100.00% CPU
  37,385,271 processor cycles
  0 bytes consed
  
*** Loaded roaring-bitmap has 1,000,000 entries.
*** containsp
Evaluation took:
  0.023 seconds of real time
  0.022552 seconds of total run time (0.022552 user, 0.000000 system)
  100.00% CPU
  63,384,344 processor cycles
  0 bytes consed
  
*** map-bitmap via cl-roaring iterator
Evaluation took:
  0.010 seconds of real time
  0.009979 seconds of total run time (0.009979 user, 0.000000 system)
  100.00% CPU
  28,053,358 processor cycles
  0 bytes consed
  

*** FSET TEST ***

*** Load iset from previously created 1000000 test values
Evaluation took:
  1.535 seconds of real time
  1.547193 seconds of total run time (1.491619 user, 0.055574 system)
  [ Run times consist of 0.174 seconds GC time, and 1.374 seconds non-GC time. ]
  100.78% CPU
  4,308,590,183 processor cycles
  1,519,755,488 bytes consed
  
*** Loaded fset has 1,000,000 entries.
*** containsp
Evaluation took:
  0.403 seconds of real time
  0.403183 seconds of total run time (0.403183 user, 0.000000 system)
  100.00% CPU
  1,132,390,144 processor cycles
  0 bytes consed

*** ordered iteration with do-set 
Evaluation took:
  0.005 seconds of real time
  0.005562 seconds of total run time (0.005527 user, 0.000035 system)
  120.00% CPU
  15,634,658 processor cycles
  0 bytes consed


|#
