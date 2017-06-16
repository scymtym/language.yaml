;;;; package.lisp --- Package definition for unit tests of the construct module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.yaml.construct.test
  (:use
   #:cl

   #:fiveam

   #:language.yaml.construct)

  (:export
   #:run-tests)

  (:documentation
   "Unit tests for the construct module."))

(cl:in-package #:language.yaml.construct.test)

(def-suite language.yaml.construct
  :description
  "Root test suite for the construct module.")

(defun run-tests ()
  (run! 'language.yaml.construct))
