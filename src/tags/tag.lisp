;;;; tag.lisp --- Tag class.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.tags)

(defclass tag (print-items:print-items-mixin)
  ((name          :initarg  :name
                  :type     string
                  :reader   name
                  :documentation
                  "The name of the tag.")
   (kind          :initarg  :kind
                  :type     node-kind
                  :reader   kind
                  :documentation
                  "The kind of nodes to which this tag is applicable.")
   (documentation :initarg  :documentation
                  :type     (or null string)
                  :reader   documentation
                  :initform nil
                  :documentation
                  "Description of the tag."))
  (:default-initargs
   :name (more-conditions:missing-required-initarg 'tag :name)
   :kind (more-conditions:missing-required-initarg 'tag :kind))
  (:documentation
   "A named tag applicable to a particular kind of nodes."))

(defmethod print-items:print-items append ((object tag))
  `((:name ,(name object) "~S")
    (:kind ,(kind object) " ~(~A~)" ((:after :name)))))
