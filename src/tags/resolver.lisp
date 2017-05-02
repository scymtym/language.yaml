;;;; resolver.lisp --- Tag resolution mechanism.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.tags)

(defclass standard-expander ()
  ((shorthands :initarg  :shorthands
               :reader   shorthands
               :initform (alist-hash-table
                          '(("!"  . "!")
                            ("!!" . "tag:yaml.org,2002:"))
                          :test #'equal))))

(defmethod expand-shorthand ((expander standard-expander)
                             (prefix   (eql :primary))
                             (suffix   string))
  (concatenate 'string (gethash "!" (shorthands expander)) suffix))

(defmethod expand-shorthand ((expander standard-expander)
                             (prefix   (eql :secondary))
                             (suffix   string))
  (concatenate 'string (gethash "!!" (shorthands expander)) suffix))

(defmethod expand-shorthand ((expander standard-expander)
                             (prefix   string)
                             (suffix   string))
  (concatenate 'string (gethash prefix (shorthands expander)) suffix))

;;;

(defmethod resolve-tag ((resolver t)
                        (kind     (eql :scalar))
                        (value    t)
                        (implicit t))
  )

(defmethod resolve-tag ((resolver t)
                        (kind     (eql :sequence))
                        (value    t)
                        (implicit ?))
    )

(defmethod resolve-tag ((resolver t)
                        (kind     (eql :mapping))
                        (value    t)
                        (implicit ?))
    )
