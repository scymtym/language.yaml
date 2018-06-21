;;;; package.lisp --- Package definition for the language.yaml system.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
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

  ;; Construct protocol
  (:export
   #:make-directive-node)

  (:export
   #:make-native-builder
   #:native-builder)

  (:documentation
   "TODO"))
