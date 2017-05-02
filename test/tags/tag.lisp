;;;; package.lisp --- Package definition for unit tests of the language.yaml system.
;;;;
;;;; Copyright (C) 2013, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.tags.test)

(in-suite language.yaml.tags)

(test tag.construction/smoke

  (finishes (make-tag "foo" :scalar)))

(test tag.print-object/smoke

  (is (search "\"foo\" scalar" (princ-to-string (make-tag "foo" :scalar)))))
