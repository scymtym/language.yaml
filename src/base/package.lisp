;;;; package.lisp --- Package definition for the base module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.yaml.base
  (:use
   #:cl)

  ;; Types
  (:export
   #:node-kind

   #:scalar-block-style
   #:scalar-flow-style
   #:scalar-style

   #:collection-block-style
   #:collection-flow-style
   #:collection-style)

  (:documentation
   "Basic type definitions used in the language.yaml system."))
