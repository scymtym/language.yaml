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

  ;; Tag Protocol
  (:export
   #:name
   #:kind

   #:make-tag)

  ;; Protocol
  (:export
   #:find-tag)

  ;; Tag resolution protocol
  (:export
   #:expand-shorthand
   #:find-shorthand)

  (:documentation
   "TODO"))
