;;;; conditions.lisp --- Conditions signaled by the tags module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.tags)

(define-condition tag-missing-error (error)
  ((uri :initarg :uri
        :type    string
        :reader  tag-condition-uri))
  (:default-initargs
   :uri (more-conditions:missing-required-initarg 'tag-condition :uri))
  (:report
   (lambda (condition stream)
     (format stream "~@<~S does not designate a known tag.~@:>"
             (tag-condition-uri condition)))))

(define-condition tag-resolution-error (error)
  ((tag          :initarg :tag
                 :reader  tag)
   (node-path    :initarg :node-path
                 :reader  node-path)
   (node-kind    :initarg :node-kind
                 :reader  node-kind)
   (node-content :initarg :node-content
                 :reader  node-content))
  (:default-initargs
   :tag (more-conditions:missing-required-initarg 'tag-resolution-error :tag))
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o tag node-path node-kind node-content) condition))
       (format stream "~@<Could not resolve tag ~A for node~@:_~
                       ~2@Tpath    ~:[«root»~;~:*~{~A~^ » ~}~]~@:_~
                       ~2@Tkind    ~(~A~)~@:_~
                       ~2@Tcontent ~S~@:>"
               (or tag "non-specific \"?\" (i.e. untagged)")
               node-path node-kind node-content)))))
