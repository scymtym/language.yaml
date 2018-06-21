;;;; native-builder.lisp --- Tests for the native builder.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.construct.test)

(in-suite :language.yaml.construct)

(test directive
  "Test processing directives."

  (bp:with-builder ((make-native-builder))
    (bp:make-node* :directive :name :tag :handle "!" :prefix "!foo")))
