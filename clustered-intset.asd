(in-package :cl-user)

(defpackage :clustered-intset-asd
  (:use :cl :asdf))

(in-package :clustered-intset-asd)

(defsystem :clustered-intset
  :version "EXPERIMENTAL-1"             ;no semantic versioning yet
  :license "MIT"
  :author "Dave Tenny"
  :description "Implements a non-negative keyed set of integers favoring clustered keys."
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "clustered-intset")))
