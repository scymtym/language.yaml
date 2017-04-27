;;;; package.lisp --- Package definition for unit tests of the base module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.yaml.base.test
  (:use
   #:cl

   #:fiveam

   #:language.yaml.base)

  (:export
   #:run-tests)

  (:documentation
   "Unit tests for the base module."))

(cl:in-package #:language.yaml.base.test)

(def-suite language.yaml.base
  :description
  "Root test suite for the base module.")

(defun run-tests ()
  (run! 'language.yaml.base))
