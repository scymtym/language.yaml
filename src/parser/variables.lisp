;;;; variables.lisp --- Variables used by the parser module.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.parser)

;;; Parser state

(declaim (type flow-state *c*))
(defvar *c* :flow-out ; TODO initial value?
  "Stores the current context state of the parser.

   See documentation of the `flow-state' for more information.")

(declaim (type (integer -1) *n*))
(defvar *n* -1
  "Stores the current indentation level.")

(declaim (type chomping-style *chomping-style*))
(defvar *chomping-style*) ; global value should never be accessed
(setf (documentation '*chomping-style* 'variable)
      "Stores the chomping style for the current block scalar.")
