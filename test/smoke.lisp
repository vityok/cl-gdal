;; Smoke-testing the bindings for the OGR library

;; sbcl --load 'smoke.lisp' --eval '(ogr-smoke-test:run)' --quit

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-ogr)
  (ql:quickload :lisp-unit))

(defpackage :ogr-smoke-test
  (:use :cl :lisp-unit)
  (:export :run))

(in-package :ogr-smoke-test)

;; --------------------------------------------------------

(define-test basic-test

    )

;; --------------------------------------------------------

(setf lisp-unit:*print-summary* T
      lisp-unit:*print-failures* T
      lisp-unit:*print-errors* T)
(lisp-unit:use-debugger T)

(defun run ()
  (ogr:ogr-register-all)
  (lisp-unit:print-errors
   (lisp-unit:run-tests :all (find-package 'ogr-smoke-test))))

;; EOF
