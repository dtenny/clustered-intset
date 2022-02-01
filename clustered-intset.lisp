;;;; See LICENSE file, and README.md for documentation.

;;;; On Performance:
;;;; 1. There are potentially some faster ways to examine 1 bits in values by applying
;;;;    masks to half-words and things.  Suggestions welcome.
;;;; 2. FIRST/NEXT functions are for convenience, not performance. If you want to iterate
;;;;    more efficiently without calling one of the MAP functions (which will examine all values)
;;;;    the ITERATOR/ADVANCE functions will be more efficient.
;;;; 3. There's probably a couple of declarations I have using VALUE-TYPE that would more 
;;;;    properly be declared FIX-MOD-TYPE, but it probably won't buy you anything 
;;;;    generated code improvements.

;;;; The INTSET and ITERATOR objects are not thread safe, however concurrent queries
;;;; should be fine assuming the lisp hash table implementation is thread safe in this regard.

(in-package :clustered-intset)

(defconstant +fixnum-bits+ (logcount MOST-POSITIVE-FIXNUM)) ; 62 in SBCL 2.0.8

(deftype key-type () 
  "Type of keys stored in the hashtable."
  '(integer 0 *))

(deftype value-type ()
  "Type of values stored in the hashtable."
  `(integer 0 ,most-positive-fixnum))

(deftype fix-mod-type ()
  "Type of value resulting from `(mod k +fixnum-bits+)`.
Generally used to specify the position of a bit in fixnum used as a bitvector."
  `(integer 0 (,+fixnum-bits+)))

(defstruct (intset (:constructor %make-intset))
  (count 0 :type integer)
  (ht (make-hash-table :test #'eql) :type hash-table :read-only t))

(defun make-intset (&key (size 7) (rehash-size 1.5))
  "Create an empty int-set.

  If you'd like to create an intset with an input sequence see SEQ->INTSET.

  :size - a non-negative integer hint as to how many keys (or key clusters) will 
          be in the underlying hashtable.  Remember that if you have good clustering,
          you might have (/ N +FIXNUM-BITS+) keys, so be careful not to overallocate.
          You are NOT declaring an equivalent number of keys as you would with a hash-table.

  :rehash-size - indicates how to expand the table when it fills up.
    If an integer, add
    space for that many elements. If a floating point number (which must be
    greater than 1.0), multiply the size by that amount."
  (%make-intset :ht (make-hash-table :size size :rehash-size rehash-size :test #'eql)))

(defun seq->intset (seq)
  "Create and return an intset populated from the specified sequence (vector or list).
Makes a rough guess about the number of hash table keys you need (HT capacity)
based on the range, or quantity, of sequence elements."
  (let* ((length (etypecase seq
                      (list (list-length seq))
                      (vector (cl:length seq))))
         (first (if (> length 0) (elt seq 0) 0))
         (min (reduce #'min seq :initial-value first))
         (max (reduce #'max seq :initial-value first))
         ;; If the range is wide and there are more seq members than the range
         ;; facter in the (/ k fixnum-bits) logic, minus 10% for anti-cluster sparsity.
         (capacity (ceiling (/ (- max min) (* +fixnum-bits+ 0.90))))
         ;; If there are fewer elements in the seq than the capacity derive from the range
         ;; use that value.
         (capacity (if (< length capacity) length capacity))
         (intset (make-intset :size capacity)))
    (map 'nil #'(lambda (i) (add i intset)) seq)
    intset))

(defun count (intset)
  "Return the count of integers held by the intset. Named for sequence-like compatibiltiy."
  (declare (type intset intset))
  ;; Requires a traversal of all values, or caching a count.
  (intset-count intset))

(defmacro with-hash-kv (vars k intset &body body)
  "Given an intset and a non-negative integer member k in the intset, bind vars in the list vars
as follows:
   first var  - the hashtable key, effectively (/ k +fixnum-bits+).
   second var - the hashtable value for the hashtable key, or zero if it doesn't exist.
   third var  - the modulus of (/ k +fixnum-bits+) which is the bit position for k in hash-value.
and execute 'body' with those bindings."
  (unless (and (listp vars) (every #'symbolp vars))
    (error "vars is not a list of symbols: ~S" vars))
  ;; May want a        #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
  ;; for blast-off mode, compiler notes (not warns) that it can't use multiplication 
  ;; hacks to divide because 'k' isn't bounded to 64 bits.
  (destructuring-bind (ht-key-var ht-value-var bit-position-var) vars
    `(locally (declare (type key-type k) (type intset intset))
       (multiple-value-bind (,ht-key-var ,bit-position-var)
           (floor ,k +fixnum-bits+)
         (declare (type key-type ,ht-key-var) (type value-type ,bit-position-var))
         (let ((,ht-value-var (gethash ,ht-key-var (intset-ht ,intset)  0)))
           (declare (type value-type ,ht-value-var))
           ,@body)))))
 
(defun add (k intset)
  "Add non-negative integer 'k' to the the intset previously created with `make-intset`.
Return true if 'k' was added, nil if 'k' was already present."
  (declare (type key-type k) (type intset intset))
  (with-hash-kv (ht-key ht-value bit-position) k intset
    (if (logbitp bit-position ht-value)
        nil                             ;k is already present
        (progn (setf (gethash ht-key (intset-ht intset)) (logxor ht-value (ash 1 bit-position)))
               (incf (intset-count intset))
               t))))

(defun delete (k intset)
  "Remove non-negative integer 'k' from the intset previously created with `make-intset`.
Return true if 'k' was removed, nil if 'k' was already absent."
  (declare (type key-type k) (type intset intset))
  (with-hash-kv (ht-key ht-value bit-position) k intset
    (if (logbitp bit-position ht-value)
        ;; k is present
        (let ((new-value (logxor ht-value (ash 1 bit-position))))
          (if (= new-value 0)
              (remhash ht-key (intset-ht intset))
              (setf (gethash ht-key (intset-ht intset)) new-value))
          (decf (intset-count intset))
          t)
        nil)))                          ;k is absent

(defun containsp (k intset)
  "Return T if the non-negative integer 'k' is a member of the specified intset, NIL otherwise."

  ;; Considered a `member` operation, but that isn't really what this does
  ;; and conses/sequence abstractions don't have another membership test. FIND perhaps?

  ;; There might be a potentially useful case for `member` semantics though if we're providing
  ;; sorted interfaces, in this case `(member k intset)` would return a sequence
  ;; of values starting with key, and all increasing values in order from there.

  (with-hash-kv (ht-key ht-value bit-position) k intset
    ;; Could check for intset-count being non-zero, assuming typical use against a populated
    ;; intset, it probably isn't really a net optimization.
    (and (> ht-value 0)                 ;no HT entry if zero
         (logbitp bit-position ht-value))))

;;; Note that we have not done a WITH-INTSET-ITERATOR in the style of WITH-HASH-TABLE-ITERATOR
;;; Because it's (surprisingly) more work than I was prepared for.

(defun map-intset (intset f)
  "Call `f`, a function of one argument, with each member of the intset.

The order of element traversal is undefined. If you wanted ascending or descending
traversal, see `map-sorted-inset`.

Behavior is undefined if the function modifies the intset, or if concurrent updates are
made from another thread.

Returns nil."
  (declare (type intset intset) (type function f))
  (with-hash-table-iterator (next-entry (intset-ht intset))
    (loop (multiple-value-bind (more? key value) 
              (next-entry)
            (unless more? (return nil)) ;no more HT keys
            ;; Call f for each 1-bit in value associated with key.
            ;; Presently doing so in ascending order (but note that intset HT keys are not
            ;; traversed in ascending order - so this is not a full ordered traversal of the
            ;; intset.)
            (locally (declare (type key-type key) (type value-type value))
              (loop for bit-position fixnum from 0 below (integer-length value)
                    with base = (* key +fixnum-bits+)
                    when (logbitp bit-position value)
                      do (funcall f (+ base bit-position))))))))

(defun map-sorted-intset (intset f direction)
  "Call `f`, a function of one argument, with each member of the inset in the ascending
or descending order according to the value of `direction`, which must be :ascending or 
:descending.

Note that this function is less efficient than unordered traversals of `map-intset`
as it requires a sort on the intset's hash table keys (and only those keys).
However the behavior is equally efficient to the unordered traversal within the 
range of up to clustered-intset:+fixnum-bits+ values that might be associated with a
hash table key.

Behavior is undefined if the function modifies the intset, or if concurrent updates are
made from another thread.

Returns nil."
  (declare (type intset intset) (type function f))
  (let* ((ht (intset-ht intset))
         (keys (sort (the list (alexandria:hash-table-keys ht))
                     (ecase direction
                       (:ascending #'<)
                       (:descending #'>)))))
    (declare (type hash-table ht) (type list keys))
    (dolist (key keys)
      (declare (type key-type key))
      (let ((value (gethash key ht)))
        (declare (type value-type value))
        (if (eq direction :ascending)
            (loop for bit-position fixnum from 0 below (integer-length value)
                  with base = (* key +fixnum-bits+)
                  when (logbitp bit-position value)
                  do (funcall f (+ base bit-position)))
            (loop for bit-position fixnum from (1- (integer-length value)) downto 0
                  with base = (* key +fixnum-bits+)
                  when (logbitp bit-position value)
                  do (funcall f (+ base bit-position))))))))

(defun intset->list (intset &key (direction :unordered))
  "Accumulate all members of the intset into a list and return it.

DIRECTION may be one of: 

  :UNORDERED (the default), which produces unordered results as efficiently as possible.
  :ASCENDING, which produces intset members in ascending order.
  :DESCENDING, which produces intset members in descending order."
  (let* ((result nil)
         (pusher #'(lambda (i) (push i result))))
    (ecase direction
      (:unordered (map-intset intset pusher))
      ;; Reverse the order to accomodate PUSH ordering
      (:ascending (map-sorted-intset intset pusher :descending))
      (:descending (map-sorted-intset intset pusher :ascending)))
    result))

(defun intset->vector (intset vector &key (direction :unordered))
  "Accumulate all members of the intset into a vector and return it.

VECTOR may be one of:
   - nil, in which case a SIMPLE-VECTOR will be allocated, filled, and returned.
   - a SIMPLE-VECTOR (i.e. `simple-vector-p` is true for the vector)
     in which case elements are added from slots zero to N-1
     where N is the number of elements in the intset. It is an error to specify an array
     of insufficient size in this case.
   - a non-adjustable vector with a fill-pointer, in which case elements are added as by
     `vector-push`.  Note that vector-push does not yield errors if the array would overflow,
     it does nothing.
   - a adjustable vector with a fill pointer, in which case elements are added as by 
     `vector-push-extend`.

Note that you can determine the size of the array to preallocate in full by allocating
`(count intset)` elements.

DIRECTION may be one of: 

  :UNORDERED (the default), which produces unordered results as efficiently as possible.
  :ASCENDING, which produces intset members in ascending order.
  :DESCENDING, which produces intset members in descending order."
  ;; Okay, okay, you got me, it's DWIM programming for no obvious benefit.
  ;; I was trying to appease my future self.
  (let* ((result (or vector (make-array (count intset))))
         (fill-pointer 0)
         (sv-pusher #'(lambda (i) 
                        (setf (svref result fill-pointer) i)
                        (incf fill-pointer)))
         (pusher (cond 
                   ((eq vector nil) sv-pusher)
                   ((simple-vector-p vector) sv-pusher)
                   ;; This is unreachable in SBCL because fill pointered arrays are also
                   ;; "actually adjustable" even if you didn't request :adjustable t.  ABCL
                   ;; behaves the same as SBCL but without compilation "unreachable code"
                   ;; notes. CLISP does not consider the array adjustable if you give it a fill
                   ;; pointer.  So the vector-push case is reachable there and in theory the
                   ;; code should exist for other lisps.  The the whole vector-push DWIM thing
                   ;; is probably too stupid to leave in the code.
                   ((and (array-has-fill-pointer-p vector) (not (adjustable-array-p vector)))
                    #'(lambda (i) (vector-push i result)))
                   ((and (array-has-fill-pointer-p vector) (adjustable-array-p vector))
                    #'(lambda (i) (vector-push-extend i result)))
                   (t (error "~s be null, a simple-vector, or an optionally-adjustable fill-pointered array." vector)))))
    ;; Muffle the compiler note above, don't want to see it, leaving the unreachable code
    ;; for portability.
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

    (ecase direction
      (:unordered (map-intset intset pusher))
      ;; Unlike intmap->list, no need to reverse order of pushing into vector
      (:ascending (map-sorted-intset intset pusher :ascending))
      (:descending (map-sorted-intset intset pusher :descending)))
    result))

;;; A word about `first` and `next`.
;;; Doing ordered searches on the hashtable ss not what you want to be doing.
;;; The only mitigating factor, and the reason I added this for experimentation purposes
;;; is that if you have clustered keys so that there are many intset members associated
;;; with a single hash key, then for N intset members, only N/62 searches are O(N).
;;; while multiple `next` calls on the same hash key to be approximately O(k) for k <= 62.
;;; `first` will always be O(N).

(defun next-value-for-ht-key (ht-key ht-value bit-position)
  "Given a hash-table key, the value from the hash table, and the bit position of some set member
stored in that key/value pair, return the next ascending value available for the 
ht key/value pair or NIL if there is no next value.

HT-VALUE may be zero, in which case we assume there are no valid keys for the value and you just
happened to have obtained it via `with-hash-kv`.  

BIT-POSITION may be nil if this is the first search for a value in HT-VALUE
otherwise it is deemed to be the position or an intset member for which we want to find
a successor value, i.e. the search starts at (1+ bit-position)."
  (declare (type key-type ht-key) (type value-type ht-value) 
           (type (or null fix-mod-type) bit-position))
  (if (= 0 ht-value)
      nil
      ;; From intset:min
      ;; TBD: Consider  [ x & -x ] to extract the rightmost one
      ;; (let ((rightmost1 (logand bs (- bs))))
      ;;   (1- (integer-length rightmost1)))
      (loop with start fixnum = (if bit-position (1+ bit-position) 0)
            for  bit-position fixnum from start below +fixnum-bits+
            when (logbitp bit-position ht-value)
            return (+ (* ht-key +fixnum-bits+) bit-position))))

(defun next-ascending-key (ht-key ht)
  "Given a hash-table HT, and a key which may or may not be that hash-table,
return the next key (with multiple values) in the hashtable with a value greater than `ht-key`,
or nil if there is no such key.

HT-KEY may be -1, which is not usually accepted in this module, for a a 'first' like
behavior that will find the lowest valued key in the intset, there are any keys.

Returns (values nil nil) or (values next-key value-for-key)."
  ;; Note, we go to the trouble to return ht-key and value together to avoid a subsequent
  ;; `gethash` by the caller if we return a non-nil key, since it isn't possible using 
  ;; Common Lisp hash-tables to examine only keys or only values, so we have both handy
  ;; when we're iterating.
  (declare (type (integer -1 *) ht-key) (type hash-table ht))
  (let ((result-key nil)
        (result-value nil))
    (declare (type (or null key-type) result-key) (type (or null value-type) result-value))
    (maphash #'(lambda (k v)
                 (declare (type key-type k) (type value-type v))
                 ;; least k above ht-key
                 (when (> k ht-key)
                   (if result-key
                       (when (< k result-key)
                         (setf result-key k result-value v))
                       (setf result-key k result-value v))))
             ht)
    (values result-key result-value)))

(defun first (intset)
  "Return the smallest valued integer in the intset, or NIL if there are no members.
This is not an efficient implementation, it is O(n)."
  (declare (type intset intset))
  (multiple-value-bind (ht-key ht-value) 
      (next-ascending-key -1 (intset-ht intset))
    (when ht-key
      (next-value-for-ht-key (the key-type ht-key) (the value-type ht-value) nil))))

(defun next (k intset)
  "For a given non-negative integer k, which does not have to be a member of the intset,
find the next larger member of the intset.  Return NIL if there is no appropriate member.

This is not an efficient implementation, this is O(n) and is a misguided convenience function.
Consider using the ITERATOR capability instead if that's what you're attempting to do,
or one of the mapping functions."
  (declare (type key-type k) (type intset intset))
  (with-hash-kv (ht-key ht-value bit-position) k intset
    ;; Check first to see if there are more values associated with the ht-key
    (or (next-value-for-ht-key ht-key ht-value bit-position)
        ;; Pick first value from next ht-key, if any
        (multiple-value-bind (next-ht-key next-ht-value)
            (next-ascending-key ht-key (intset-ht intset))
          (declare (type (or null key-type) next-ht-key) (type (or null value-type) next-ht-value))
          (when next-ht-key
            (next-value-for-ht-key (the key-type next-ht-key) (the value-type next-ht-value) nil))))))

;;;
;;; Iterator logic
;;;

(defstruct iterator
  "An object representation iteration state over an INTSET"
  (ht nil :type hash-table)             ;the intset hash-table
  (intset nil :type intset)    ;just for debugging so you can see which intset iterator is for.
  (descending nil)
  (starting-with nil)
  (value-qualifies-fn nil :type function) ; predicate for intset values w.r.t starting/ending-with
  (ending-with nil)
  ;; Whether or not ADVANCE *may* produce values depends on either remaining-ht-keys
  ;; having values, or active still being true (in which case we're examining values
  ;; for current-ht-key and there are no other HT keys remaining).
  (remaining-ht-keys nil :type (or null vector))
  (state nil :type keyword)            ;:init :active :done
  ;; The following are meaningless until `active` is not :init
  (current-ht-key 0 :type key-type)
  (current-ht-value 0 :type value-type)
  (current-bit-position 0 :type fix-mod-type))

(defun some-ht-val (ht k pred)
  "For some intset member K, check to see if there is some value in the hashtable value bits
associated with the hashtable key for K `(floor k fixnum-bits)`, for which 
pred is true. For example:

    (some-ht-val ht 43 #'<)

For 62 fixnum bits, would check ht key 0 for any ht value bits indicating quantities < 43.
Similarly:

    (some-ht-val ht 143 #'<=)

For 62 fixnum bits would check for values associated with that key from (* 2 62) to 
(* 3 62) that were <= 143.

If we find a PRED-qualified value, return true, otherwise return nil."
  (declare (hash-table ht) (key-type k) (function pred))
  (multiple-value-bind (hash-key bit-position)
      (floor k +fixnum-bits+)
    (declare (key-type hash-key) (fix-mod-type bit-position))
    (let ((hash-value (gethash hash-key ht)))
      (and 
       hash-value
       ;; We don't try to optimize for ascending or descending for now, 
       ;; a better optimization would probably be to optimize by testing half-words for non-zero
       ;; or something like that.  
       (loop for current-bit-position fixnum from 0 below (integer-length hash-value)
             when (and (logbitp current-bit-position hash-value)
                       (funcall pred current-bit-position bit-position))
             do (return t))))))

(defun ht-key-filter (ht starting-with ending-with descending)
  "Return a function which acts as a filter for hash table keys
to see if they quality for iteration according to `iterator` semantics below."
  (declare (hash-table ht)
           (type (or null key-type) starting-with)
           (type (or null key-type) ending-with))
  ;; Starting/ending-with are intset values.
  ;; key-starting/ending-with are ht key values.
  (let ((key-starting-with (and starting-with (floor starting-with +fixnum-bits+)))
        (key-ending-with (and ending-with (floor ending-with +fixnum-bits+))))
    (cond ((and starting-with ending-with)
           (locally (declare (key-type starting-with ending-with
                                       key-starting-with key-ending-with))
             (if descending
                 #'(lambda (k) 
                     (declare (key-type k))
                     (or (and (< k key-starting-with)
                              (> k key-ending-with))
                         (and (= k key-starting-with)
                              (some-ht-val ht starting-with #'<=))
                         (and (= k key-ending-with)
                              (some-ht-val ht ending-with #'>))))
                 #'(lambda (k)
                     (declare (key-type k))
                     (or (and (> k key-starting-with)
                              (< k key-ending-with))
                         (and (= k key-starting-with)
                              (some-ht-val ht starting-with #'>=))
                         (and (= k key-ending-with)
                              (some-ht-val ht ending-with #'<)))))))

        (starting-with
         (locally (declare (key-type starting-with))
           (if descending
               #'(lambda (k) 
                   (declare (key-type k))
                   (or (< k key-starting-with)
                       (and (= k key-starting-with)
                            (some-ht-val ht starting-with #'<=))))
               #'(lambda (k)
                   (declare (key-type k))
                   (or (> k key-starting-with)
                       (and (= k key-starting-with)
                            (some-ht-val ht starting-with #'>=)))))))

        (ending-with
         (locally (declare (key-type ending-with))
           (if descending
               #'(lambda (k) 
                   (declare (key-type k))
                   (or (> k key-ending-with)
                       (and (= k key-ending-with)
                            (some-ht-val ht ending-with #'>))))
               #'(lambda (k)
                   (declare (key-type k))
                   (or (< k key-ending-with)
                       (and (= k key-ending-with)
                            (some-ht-val ht ending-with #'<)))))))

        (t ;; all ht keys welcome
         #'(lambda (k) (declare (ignore k)) t)))))
  
(defun value-predicate (starting-with ending-with descending)
  "Similar to `ht-key-filter`, only in thise case we're looking at intset members values
with the optional iteration controls (starting-with, ending-with, descending).  Return a
predicate which accepts an intset member value and returns true if an intset member qualifies,
w.r.t. the optional range value, nil if it does not."
  (declare (type (or null key-type) starting-with)
           (type (or null key-type) ending-with))
  (cond ((and starting-with ending-with)
         (locally (declare (key-type starting-with ending-with))
           (if descending
               #'(lambda (k) 
                   (declare (key-type k))
                   (and (<= k starting-with)
                        (>  k ending-with)))
               #'(lambda (k)
                   (declare (key-type k))
                   (and (>= k starting-with)
                        (<  k ending-with))))))
               
        (starting-with
         (locally (declare (key-type starting-with))
           (if descending
               #'(lambda (k)
                   (declare (key-type k))
                   (<= k starting-with))
               #'(lambda (k)
                   (declare (key-type k))
                   (>= k starting-with)))))

        (ending-with
         (locally (declare (key-type ending-with))
           (if descending
               #'(lambda (k)
                   (declare (key-type k))
                   (> k ending-with))
               #'(lambda (k)
                   (declare (key-type k))
                   (< k ending-with)))))

        (t #'(lambda (k) (declare (ignore k)) t))))

(defun iterator (intset &key starting-with ending-with descending)
  "Creates and returns an unidirectional iterator for ordered traversal of the intset.
The iterator is called with `(advance <iterator>)` until it returns nil.

E.g.  (loop with iterator = (iterator intset)
            as value = (advance iterator)
            while value
            do ... something with value ...)

Values returned by ADVANCE will be ascending or descending according to the value of
DESCENDING. By default, iteration proceeds with all intset values in ascending order.

If DESCENDING is true, iterataion proceeds in descending order. As explained below,
DESCENDING affects comparison of STARTING-WITH and ENDING-WITH values.
A single iterator is always unidirectional

STARTING-WITH, if specified, is an inclusive value limiting the iteration to
start to the first value equal to or (> if DESCENDING, < if not DESCENDING) the value.

ENDING-WITH, is specified, is an exclusive value limiting the iteration
range end to the first value (> if DESCENDING, < if not DESCENDING) the value.

If both STARTING-WITH and ENDING-WITH are supplied and overlap in semantically incompatible
ways an error will be signalled, e.g.

  `(iterator intset :starting-with 1 :ending-with 1)` => ERROR

The intset from which the iterator was created may be updated while iterating.
It is undefined whether newly added values will be produced by new calls to ADVANCE.
If a member k is deleted from the intset undergoing iteration after the iterator
is created, it will not appear in any ADVANCE calls after deletion."
  (declare (type intset intset) 
           (type (or null key-type) starting-with)
           (type (or null key-type) ending-with))
  (when (and starting-with ending-with
             (or (and descending (>= ending-with starting-with))
                 (and (not descending) (>= starting-with ending-with))))
    (error "STARTING-WITH value ~d and ENDING-WITH value ~d are incompatible with DESCENDING ~a."
           starting-with ending-with DESCENDING))
  ;; Could make initial ht-key-vector smaller by some application of non-nil -WITH values,
  ;; as we could be substantially overallocating in that case. TODO.
  (let ((ht-key-vector (make-array (hash-table-count (intset-ht intset)) 
                                   :element-type 'key-type
                                   :fill-pointer 0))
        (ht-key-filter-fn (ht-key-filter (intset-ht intset) 
                                         starting-with ending-with descending)))
    ;; Determine which ht keys are of interest and put them in the correct order
    (alexandria:maphash-keys 
     #'(lambda (k) (when (funcall ht-key-filter-fn k)
                     (vector-push k ht-key-vector)))
     (intset-ht intset))
    ;; Sort the vector according to how we'd like to vector-pop the elements off the end
    (sort ht-key-vector (if descending #'< #'>))
    (let ((nothing? (= 0 (cl:length ht-key-vector))))
      (make-iterator :ht (intset-ht intset)
                     :intset intset
                     :descending descending
                     :value-qualifies-fn (value-predicate starting-with ending-with descending)
                     :ending-with ending-with
                     :starting-with starting-with
                     :remaining-ht-keys (if nothing? nil ht-key-vector)
                     :state :init))))

(defun iterator-next-bit-value (iterator)
  "For an active iterator, if there's another value to be retrieved for the current 
hash-table key, advance the iterator to that bit and return the value. Otherwise return nil."
  (declare (iterator iterator))
  (let* ((hash-key (iterator-current-ht-key iterator))
         (hash-value (iterator-current-ht-value iterator))
         (base (* hash-key +fixnum-bits+))
         (value-qualifies-fn (iterator-value-qualifies-fn iterator)))
    (declare (key-type hash-key base) (value-type hash-value) (function value-qualifies-fn))
    (if (iterator-descending iterator)
        ;; Only the `for` varies based on `desending`.
        (loop for bit-position fixnum from (1- (iterator-current-bit-position iterator)) downto 0
              as  value = (and (logbitp bit-position hash-value)
                               (+ base bit-position))
              when (and value (funcall value-qualifies-fn value))
              do   (setf (iterator-current-bit-position iterator) bit-position)
                   (return value))
        (loop for bit-position fixnum from (1+ (iterator-current-bit-position iterator)) below (integer-length hash-value)
              as  value = (and (logbitp bit-position hash-value)
                               (+ base bit-position))
              when (and value (funcall value-qualifies-fn value))
              do   (setf (iterator-current-bit-position iterator) bit-position)
                   (return value)))))

(defun initiate-new-ht-key (hash-key iterator)
  "We're setting up for the first HT key in iteration, or moving to a new HT key.
Set the iterator state to reflect this HT key, and find the first bit position
with a relevant value.  Assuming the iterator initialization filtered any keys without
any applicable values, we'll always return a value for a new HT key here.

Does not maintain the remaining-ht-keys slot of the iterator.
Does update the 'current' values iterator.

Returns the intset element value for the first value associated with the HT key, which
should not be nil."
  (declare (key-type hash-key) (iterator iterator))
  (let ((hash-value (gethash hash-key (iterator-ht iterator)))
        (base (* hash-key +fixnum-bits+))
        (value-qualifies-fn (iterator-value-qualifies-fn iterator)))
    (declare (key-type base) (value-type hash-value) (function value-qualifies-fn))
    (setf (iterator-current-ht-key iterator) hash-key)
    (setf (iterator-current-ht-value iterator) hash-value)

    (if (iterator-descending iterator)
        ;; Only the `for` varies based on `desending`.
        (loop for bit-position fixnum from (1- (integer-length hash-value)) downto 0
              as  value = (and (logbitp bit-position hash-value)
                               (+ base bit-position))
              when (and value (funcall value-qualifies-fn value))
              do   (setf (iterator-current-bit-position iterator) bit-position)
                   (return-from initiate-new-ht-key value))
        (loop for bit-position fixnum from 0 below (integer-length hash-value)
              as  value = (and (logbitp bit-position hash-value)
                               (+ base bit-position))
              when (and value (funcall value-qualifies-fn value))
              do   (setf (iterator-current-bit-position iterator) bit-position)
                   (return-from initiate-new-ht-key value)))

    ;; If we fell through the loop, we didn't find a value for the HT key, and that's
    ;; wrong, it should have been filtered out at iterator creation time.
    (error "~S failed to yield a value for hash table key ~d." iterator hash-key)))


(defun iterator-next-key-value (iterator)
  "For an active iterator, if there's another hash key to process, advance to that key
and return the value.  Otherwise mark the iterator inactive and return nil."
  (declare (iterator iterator))
  (alexandria:when-let (ht-keys (iterator-remaining-ht-keys iterator))
    (let ((hash-key (vector-pop ht-keys)))
      (declare (key-type hash-key))
      (when (zerop (cl:length ht-keys))
        (setf (iterator-remaining-ht-keys iterator) nil))
      (initiate-new-ht-key hash-key iterator))))
 
(defun next-value (iterator)
  "Advance an in-progress iterator if we can, returning the next value and updating iterator 
state. If there are no more values, deactivate the iterator and return nil."
  (declare (iterator iterator))
  (and (eq :active (iterator-state iterator))
       (or (iterator-next-bit-value iterator)
           (iterator-next-key-value iterator)
           (progn (setf (iterator-state iterator) :done) nil))))

(defun first-value (iterator)
  "An an iterator hasn't been activated yet, activate it and begin looking for keys.
Return the next value, if any, or nil if there isn't any.  Update iterator state accordingly."
  (declare (iterator iterator))
  (when (iterator-remaining-ht-keys iterator)
    (let* ((hash-key (vector-pop (iterator-remaining-ht-keys iterator))))
      (declare (key-type hash-key))
      (setf (iterator-state iterator) :active)
      (when (zerop (cl:length (the vector (iterator-remaining-ht-keys iterator))))
        (setf (iterator-remaining-ht-keys iterator) nil))
      (initiate-new-ht-key hash-key iterator))))

(defun advance (iterator)
  "Advance an iterator returned by `iterator`.  Return the next integer value in the iteration,
  or nil if there are no more values."
  (declare (iterator iterator))
  (ecase (iterator-state iterator)
    (:active (next-value iterator))
    (:init (first-value iterator))
    (:done nil)))
          

;; SEQUENCE abstractions
;; consider https://shinmera.github.io/trivial-extensible-sequences/
;;      and http://www.sbcl.org/manual/#Extensible-Sequences
;; merge, sort, map-into, reduce

;; CONSES abstractions
;; member, mapc..., [n]intersection, [n]set-difference, [n]set-exclusive-or, 
;; subsetp, [n]union

;; SET abstractions
;; subset-p(logand), proper-subset-p intersection(logand), union(logior), 
;; difference(logandc2, exclusive-or(logxor)

;; MISC
;; clear, equal, CTOR taking vector|list,
;; last & prev (descending sorted traversals)
;; (rename first to min, last to max)
