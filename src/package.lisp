;;;; package.lisp --- Package definition for the language.yaml system.
;;;;
;;;; Copyright (C) 2013, 2014, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.yaml
  (:use
   #:cl)

  (:shadow
   #:load)

  ;; Dump/load protocol
  (:export
   #:dump
   #:load)

  (:documentation
   "Interface package for the language.yaml system."))
