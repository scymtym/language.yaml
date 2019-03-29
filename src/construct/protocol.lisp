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

;;; Anchor/alias protocol

(defgeneric find-anchor (name container &key if-does-not-exist)
  (:documentation
   "Return the node stored for the anchor named NAME in CONTAINER.

    Return two values: 1) the node or nil if there is no anchor named
    NAME in CONTAINER. 2) a Boolean indicating whether an anchor named
    NAME exists in container.

    IF-DOES-NOT-EXIST, if supplied, controls the behavior in case an
    anchor named NAME does not exist in CONTAINER. TODO"))

(defgeneric (setf find-anchor) (new-value name container &key if-does-not-exist)
  (:documentation
   "TODO

    IF-DOES-NOT-EXIST is accepted for parity with FIND-ANCHOR and
    ignored."))
