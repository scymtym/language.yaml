;;;; conditions.lisp --- Conditions used in the construct module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.construct)

(define-condition anchor-missing-error (error)
  ((%name :initarg :name
          :reader  name))
  (:default-initargs
   :name (missing-required-initarg 'anchor-missing-error :name))
  (:report
   (lambda (condition stream)
     (format stream "~@<Alias name ~S does not refer to a defined ~
                     anchor.~@:>"
             (name condition)))))
