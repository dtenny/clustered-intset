(in-package :cl-user)

(defpackage :clustered-intset-test-asd
  (:use :cl :asdf))

(in-package :clustered-intset-test-asd)

(defsystem :clustered-intset-test
  :version "0.0.1"
  :license "MIT"
  :author "Dave Tenny"
  :description "tests for clustered-intset"
  :depends-on (:clustered-intset :fiveam :alexandria)
  :components ((:file "clustered-intset-test")))
