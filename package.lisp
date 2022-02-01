(in-package :cl-user)

(defpackage :clustered-intset
  (:use :cl)
  (:shadow #:delete #:first #:count)
  (:documentation "Hash based int-set that is slightly more efficient for clustered keys
such as a set of integer primary key values from a database.
All keys must be non-negative integers.")

  ;; These are all functions operating on intset structures unless otherwise noted.
  (:export
   #:add
   #:advance                            ;advance iterator created by `iterator`.
   #:containsp
   #:count
   #:delete
   #:first
   #:intset->list
   #:intset->vector
   #:iterator                           ;create iterator from intset
   #:make-intset
   #:map-intset
   #:map-sorted-intset
   #:next                               ;inefficient - no iterator
   #:seq->intset
   ))
                  
