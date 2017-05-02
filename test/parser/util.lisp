;;;; util.lisp --- Utilities used by the unit tests for the parser module.
;;;;
;;;; Copyright (C) 2013, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.parser.test)

;;; Grammar for test case filenames

(esrap:defrule n-binding
    (and "n=" (+ (not #\.)))
  (:function second)
  (:text t)
  (:lambda (text)
    (list '*n* (parse-integer text))))

(esrap:defrule c-binding
    (and "c=" (+ (not (or #\. (not character)))))
  (:function second)
  (:text t)
  (:lambda (text)
    (list '*c* (make-keyword (string-upcase text)))))

(esrap:defrule t-binding
    (and "t=" (+ (not (or #\. (not character)))))
  (:function second)
  (:text t)
  (:lambda (text)
    (list '*chomping-style* (make-keyword (string-upcase text)))))

(esrap:defrule binding
    (and #\. (or n-binding c-binding t-binding))
  (:function second))

(esrap:defrule rule-name
    (+ (not #\.))
  (:text t)
  (:function string-upcase)
  (:lambda (name)
    (intern name :language.yaml.parser)))

(esrap:defrule tag
    (and #\. (+ character))
  (:function second)
  (:text t))

(esrap:defrule test-case-spec
    (and rule-name (* binding) (esrap:? tag)))

(defun parse-test-file-name (name)
  (values-list (esrap:parse 'test-case-spec (pathname-name name))))

;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-data-files (type)
    (directory
     (merge-pathnames
      (make-pathname :name :wild :type type)
      (asdf:system-relative-pathname :language.yaml.parser "data/test-cases/dummy"))))

  (defun %print-without-earmuffs (stream thing colon? at?)
    (declare (ignore colon? at?))
    (write-string (ecase thing
                    (*c*              "c")
                    (*n*              "n")
                    (*chomping-style* "t"))
                  stream))

  (defun output-file-name (input rule bindings tag)
    (merge-pathnames
     (format nil "~(~A~@[~{~{.~/language.yaml.parser.test::%print-without-earmuffs/=~A~}~}~]~)~@[.~A~].output"
             rule bindings tag)
     input)))
