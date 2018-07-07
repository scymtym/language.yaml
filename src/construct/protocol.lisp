;;;; protocol.lisp --- Protocol provided by the construct module.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.construct)

(defgeneric make-directive-node (builder kind &key &allow-other-keys)
  (:documentation
   "TODO"))

(defgeneric make-node-using-tag (builder kind tag &key &allow-other-keys)
  (:documentation
   "TODO"))
