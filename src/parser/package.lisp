;;;; package.lisp --- Package definition for the parser module.
;;;;
;;;; Copyright (C) 2013, 2014, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.yaml.parser
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:architecture.builder-protocol

   #:esrap
   #:parser.common-rules)

  (:shadowing-import-from #:esrap
   #:?)

  (:import-from #:language.yaml.base
   #:scalar-style
   #:collection-style)

  (:documentation
   "TODO"))
