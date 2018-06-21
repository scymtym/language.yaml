;;;; package.lisp --- Package definition for unit tests of the language.yaml system.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.yaml.test
  (:use
   #:cl
   #:let-plus

   #:fiveam)

  (:export
   #:run-tests)

  (:documentation
   "Unit tests for the language.yaml system."))

(cl:in-package #:language.yaml.test)

(def-suite :language.yaml
  :description
  "Root test suite for the language.yaml system.")

(defun run-tests ()
  (run! :language.yaml))
