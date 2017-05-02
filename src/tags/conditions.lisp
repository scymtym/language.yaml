;;;; conditions.lisp --- Conditions signaled by the tags module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
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
