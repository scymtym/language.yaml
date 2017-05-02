;;;; package.lisp --- Package definition for unit tests of the parser module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.yaml.parser.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:fiveam

   #:language.yaml.parser)

  (:import-from #:language.yaml.parser
   #:*c* #:*n* #:*chomping-style*)

  (:export
   #:run-tests)

  (:documentation
   "Unit tests for the parser module."))

(cl:in-package #:language.yaml.parser.test)

(def-suite language.yaml.parser
  :description
  "Root test suite for the parser module.")

(defun run-tests ()
  (run! 'language.yaml.parser))
