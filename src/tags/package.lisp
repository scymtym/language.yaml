;;;; package.lisp --- Package definition for the tags module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.yaml.tags
  (:use
   #:cl
   #:alexandria

   #:language.yaml.base)

  ;; Tag Protocol
  (:export
   #:name
   #:kind

   #:make-tag)

  ;; Protocol
  (:export
   #:find-tag
   #:expand-shorthand)

  (:documentation
   "TODO"))
