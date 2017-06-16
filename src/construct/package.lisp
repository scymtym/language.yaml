;;;; package.lisp --- Package definition for the language.yaml system.
;;;;
;;;; Copyright (C) 2013, 2014, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.yaml.construct
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:import-from #:language.yaml.tags
   #:find-tag)

  (:documentation
   "TODO"))
