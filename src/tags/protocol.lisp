;;;; protocol.lisp --- Protocol provided by the tags module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.tags)

;;; Tag protocol

(defgeneric name (tag))

(defgeneric id (tag))

(defgeneric documentation (tag))

(defun make-tag (name kind &key documentation)
  (apply #'make-instance 'tag :name name :kind kind
         (when documentation
           (list :documentation documentation))))

;;; Tag registry protocol



;;; Tag resolution protocol

(defgeneric expand-shorthand (expander prefix suffix)
  (:documentation
   "TODO"))

(defgeneric resolve-tag (resolver tag node-path node-kind node-content)
  (:documentation
   "TODO"))
