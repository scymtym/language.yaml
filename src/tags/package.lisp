;;;; package.lisp --- Package definition for the tags module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.yaml.tags
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:language.yaml.base)

  (:shadow
   #:documentation)

  ;; Conditions
  (:export
   #:tag-resolution-error)

  ;; Tag Protocol
  (:export
   #:name
   #:kind
   #:documentation

   #:make-tag)

  ;; Protocol
  (:export
   #:find-tag
   #:ensure-tag

   #:define-tag)

  ;; Tag resolution protocol
  (:export
   #:expand-shorthand
   #:find-shorthand) ; also setf

  (:export
   #:standard-expander)

  (:documentation
   "TODO"))
