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

(defvar *all-tags* (make-hash-table :test #'equal))

(defun find-tag (name &key (if-does-not-exist #'error)) ; TODO name -> uri ?
  (or (gethash name *all-tags*)
      (more-conditions:error-behavior-restart-case
          (if-does-not-exist (tag-missing-error :uri name)))))

(defun (setf find-tag) (new-value name &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (setf (gethash name *all-tags*) new-value))

(defun ensure-tag (name kind &rest initargs)
  (if-let ((existing (find-tag name :if-does-not-exist nil)))
    (apply #'reinitialize-instance existing :kind kind initargs)
    (setf (find-tag name)
          (apply #'make-tag name kind initargs))))

(defmacro define-tag ((name kind) &key documentation)
  (when (constantp kind)
    (let ((kind (eval kind)))
      (check-type kind node-kind)))

  `(ensure-tag ,name ,kind ,@(when documentation
                               `(:documentation ,documentation))))

;;; Tag resolution protocol

(defgeneric expand-shorthand (expander prefix suffix)
  (:documentation
   "TODO"))

(defgeneric resolve-tag (resolver tag node-path node-kind node-content)
  (:documentation
   "TODO"))

;;; Default behavior

(defmethod resolve-tag :around ((resolver     t)
                                (tag          t)
                                (node-path    t)
                                (node-kind    t)
                                (node-content t))
  (let+ (((&values tag arguments) (call-next-method)))
    (unless tag
      (error "~@<Could not resolve tag ~A for node~@:_~
              ~2@Tpath    ~:A~@:_~
              ~2@Tkind    ~(~A~)~@:_~
              ~2@Tcontent ~S~@:>"
             (or tag "non-specific \"?\" (i.e. untagged)")
             node-path node-kind node-content))
    (values tag arguments)))
