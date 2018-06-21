;;;; package.lisp --- Package definition for unit tests of the tags module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.yaml.tags.test
  (:use
   #:cl

   #:fiveam

   #:language.yaml.tags)

  (:shadowing-import-from #:language.yaml.tags
   #:documentation)

  (:export
   #:run-tests)

  (:documentation
   "Unit tests for the tags module."))

(cl:in-package #:language.yaml.tags.test)

(def-suite language.yaml.tags
  :description
  "Root test suite for the tags module.")

(defun run-tests ()
  (run! 'language.yaml.tags))
