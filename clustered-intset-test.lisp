(in-package :cl-user)

(defpackage :clustered-intset-test
  (:use :cl :fiveam :alexandria)
  (:local-nicknames (:ci :clustered-intset))
  (:export #:run-tests)
  (:documentation "Tests for the :clustered-intset package."))

(in-package :clustered-intset-test)

(defconstant +fixnum-bits+ ci::+fixnum-bits+)

(define-constant +test-vals+
  (list 0 1 2 
        ;; bracketing some +fixnum-bits+ values
        (- +fixnum-bits+ 2) (- +fixnum-bits+ 1) 
        +fixnum-bits+
        (+ +fixnum-bits+  1) (+ +fixnum-bits+ 2)

        ;; bracketing most-positive-fixnum
        (- most-positive-fixnum 2) (- most-positive-fixnum 1) 
        most-positive-fixnum 
        (+ most-positive-fixnum 1) (+ most-positive-fixnum 2)

        ;; bracketing 2x most-positive-fixnum
        (- (* 2 most-positive-fixnum) 2) (- (* 2 most-positive-fixnum) 1) 
        (* 2 most-positive-fixnum)
        (+ (* 2 most-positive-fixnum) 1) (+ (* 2 most-positive-fixnum) 2)

        ;; Bracking some +fixnum-bits+ values on a 2x most-positive-fixnum
        (- (+ +fixnum-bits+ (* 2 most-positive-fixnum)) 2)
        (- (+ +fixnum-bits+ (* 2 most-positive-fixnum)) 1) 
        (+ +fixnum-bits+ (* 2 most-positive-fixnum))
        (+ (+ +fixnum-bits+ (* 2 most-positive-fixnum))  1)
        (+ (+ +fixnum-bits+ (* 2 most-positive-fixnum)) 2))
  :test 'equal)

(defun make-intset (&rest ints)
  "Create and return an intset with the specified values."
  (ci:seq->intset ints))

(def-suite test-suite :description "clustered-intset tests")
(in-suite test-suite)

(test basic-add-test-remove
  ;; Some very basic warmup tests
  (let ((iset (ci:make-intset)))
    ;; Add zero
    (is (ci:add 0 iset))
    (is (ci:containsp 0 iset))
    (is (not (ci:add 0 iset)))
    (is (= 1 (ci:length iset)))
    (is (not (ci:containsp 1 iset)))

    ;; Ensure removal of HT entries representing no members (no 1-bits)
    (is (ci:delete 0 iset))
    (is (= 0 (ci:length iset)))
    (is (= 0 (hash-table-count (ci::intset-ht iset))))
    (is (not (ci:containsp 0 iset))))

  ;; Now add/test/remove a variety of values 
  (let ((iset (ci:make-intset)))
    (dolist (i +test-vals+)
      (is (ci:add i iset))
      (is (ci:containsp i iset))
      (is (not (ci:add i iset))))
    (is (= (length +test-vals+) (ci:length iset)))
    (dolist (i +test-vals+)
      (is (ci:delete i iset)))
    (is (= 0 (ci:length iset)))))

(test map-intset                        ;also intset->list
  (is (equal nil (set-difference '(1 2 3) (ci:intset->list (make-intset 1 2 3)))))
  (is (equal nil (set-difference +test-vals+ 
                                 (ci:intset->list (ci:seq->intset +test-vals+))))))

(test map-intset-sorted                 ;also intset->list
  (let ((iset (ci:seq->intset +test-vals+)))
    (is (equal (sort (copy-list +test-vals+) #'<)
               (ci:intset->list iset :direction :ascending)))
    (is (equal (sort (copy-list +test-vals+) #'>)
               (ci:intset->list iset :direction :descending)))))

(defun seq-equalp (seq1 seq2)
  "Return non-nil if sequences are equal, nil otherwise."
  (and (= (length seq1) (length seq2))
       (every #'= seq1 seq2)))

(test intset->vector                    ;intset->list tested above
  (let ((iset (ci:seq->intset +test-vals+)))
    (dolist (v (list
                nil 
                (make-array (length +test-vals+))
                ;; Missing the vector-push test case, portability issues, stupid idea.
                (make-array (floor (length +test-vals+) 2) :fill-pointer 0 :adjustable t)
                ))
      (let ((result (ci:intset->vector iset v))) ;unordered output
        (is (equal nil (set-difference +test-vals+ (coerce result 'list))))
        (when (or (null v) (simple-vector-p v))
          (is (simple-vector-p result)))
        (when (and v (array-has-fill-pointer-p v))
          (is (array-has-fill-pointer-p result))
          ;; Reset the array for the next filling of it
          (is (eq v (adjust-array v (length v) :fill-pointer 0 :initial-element 0))))
        (is (seq-equalp (sort (copy-list +test-vals+) #'<)
                        (ci:intset->vector iset v :direction :ascending)))
        (when (and v (array-has-fill-pointer-p v))
          (is (eq v (adjust-array v (length v) :fill-pointer 0 :initial-element 0))))
        (is (seq-equalp (sort (copy-list +test-vals+) #'>)
                        (ci:intset->vector iset v :direction :descending)))))))

(test bogus-key-conditions
  (let ((iset (ci:seq->intset +test-vals+)))
    ;; ecase signals type-error according to hyperspec
    (signals type-error (ci:intset->list iset :direction :foo))
    (signals type-error (ci:map-sorted-intset iset #'(lambda (i) i) :bar))))

(test next
  (let ((iset (ci:seq->intset +test-vals+))) 
    (is (= 0 (ci:first iset)))
    (loop for prev-list on +test-vals+
          for current-list on (cdr +test-vals+)
          as  prev = (car prev-list)
          as  current = (car current-list)
          do (is (= current (ci:next prev iset))))))

(test iterator
  (let* ((iset (ci:seq->intset +test-vals+)))
    ;; Unrestricted range, ascending
    (let ((i (ci:iterator iset)))
      (is (equalp +test-vals+
                  (loop as v = (ci:advance i)
                        while v
                        collect v))))
    ;; Unrestricted range, descending
    (let ((i (ci:iterator iset :descending t)))
      (is (equalp (reverse +test-vals+)
                  (loop as v = (ci:advance i)
                        while v
                        collect v))))
    ;; starting-with ascending
    (loop for idx from 0 to (1- (length +test-vals+))
          do (is (equalp (nthcdr idx +test-vals+)
                         (let ((i (ci:iterator iset :starting-with (elt +test-vals+ idx))))
                           (loop as v = (ci:advance i)
                                 while v
                                 collect v)))))
    ;; ending-with ascending
    (loop for idx from 0 to (1- (length +test-vals+))
          do (is (equalp (subseq +test-vals+ 0 idx)
                         (let ((i (ci:iterator iset :ending-with (elt +test-vals+ idx))))
                           (loop as v = (ci:advance i)
                                 while v
                                 collect v)))))
    ;; starting-with descending
    (loop for idx from 0 to (1- (length +test-vals+))
          do (is (equalp (reverse (subseq +test-vals+ 0 (1+ idx)))
                         (let ((i (ci:iterator iset :starting-with (elt +test-vals+ idx)
                                               :descending t)))
                           (loop as v = (ci:advance i)
                                 while v
                                 collect v)))))
    ;; ending-with descending
    (loop for idx from 0 to (1- (length +test-vals+))
          do (is (equalp (reverse (subseq +test-vals+ (1+ idx)))
                         (let ((i (ci:iterator iset :ending-with (elt +test-vals+ idx)
                                               :descending t)))
                           (loop as v = (ci:advance i)
                                 while v
                                 collect v)))))
    ;; starting-with and ending-with ascending
    (is (equalp '(60 61 62) 
                (let ((i (ci:iterator iset :starting-with 60 :ending-with 63)))
                  (loop as v = (ci:advance i)
                        while v
                        collect v))))
    (is (equalp '(60 61 62 63 64 4611686018427387901) 
                (let ((i (ci:iterator iset :starting-with 60 :ending-with 4611686018427387902)))
                  (loop as v = (ci:advance i)
                        while v
                        collect v))))
    ;; STARTING-WITH value 9223372036854775870 and ENDING-WITH value 9223372036854775870 are 
    ;; incompatible with DESCENDING NIL..
    (signals simple-error (let ((last-val (first (last +test-vals+))))
                            (ci:iterator iset :starting-with last-val :ending-with last-val)))

    ;; starting-with and ending-with descending
    (is (equalp '(63 62 61)
                (let ((i (ci:iterator iset :starting-with 63 :ending-with 60 :descending t)))
                  (loop as v = (ci:advance i)
                        while v
                        collect v))))
    (is (equalp '(4611686018427387902 4611686018427387901 64 63 62 61)
                (let ((i (ci:iterator iset :starting-with 4611686018427387902 :ending-with 60
                                      :descending t)))
                  (loop as v = (ci:advance i)
                        while v
                        collect v))))
    ;; STARTING-WITH value 9223372036854775870 and ENDING-WITH value 9223372036854775870 are 
    ;; incompatible with DESCENDING NIL..
    (signals simple-error (let ((last-val (first (last +test-vals+))))
                            (ci:iterator iset :starting-with last-val :ending-with last-val
                                         :descending t)))
    ))
  

(defun run-tests ()
  "Run all :clustered-intset tests."
  (explain! (run 'test-suite)))


