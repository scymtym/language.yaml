;;;; mixins.lisp --- Mixins used in the construct module.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.construct)

;;; `anchor-recording-mixin'

(defclass anchor-recording-mixin ()
  ((anchors :reader   anchors ; TODO allocate the hash-table lazily?
            :initform (make-hash-table :test #'equal)) ; TODO store this in the document?
   ))

(defmethod find-anchor ((container anchor-recording-mixin)
                        (name      t))
  (gethash name (anchors container)))

(defmethod (setf find-anchor) ((new-value t)
                               (container anchor-recording-mixin)
                               (name      t))
  (setf (gethash name (anchors container)) new-value))

(defmethod bp:make-node ((builder anchor-recording-mixin)
                         (kind    (eql :alias))
                         &key
                         anchor)
  (gethash anchor (anchors builder)))

(flet ((note-node (builder anchor node)
         (when anchor
           (setf (gethash anchor (anchors builder)) node))
         node))

  (defmethod bp:make-node :around ((builder anchor-recording-mixin)
                                   (kind    (eql :scalar))
                                   &key anchor)
    (note-node builder anchor (call-next-method)))

  (defmethod bp:make-node :around ((builder anchor-recording-mixin)
                                   (kind    (eql :sequence))
                                   &key anchor)
    (note-node builder anchor (call-next-method)))

  (defmethod bp:make-node :around ((builder anchor-recording-mixin)
                                   (kind    (eql :mapping))
                                   &key anchor)
    (note-node builder anchor (call-next-method))))

;;; `tag-based-builder-mixin'

(defclass tag-based-builder-mixin ()
  ((resolver :initarg  :resolver
             :reader   resolver)))

(flet ((resolve-tag (builder tag node-path node-kind node-content)
         (etypecase tag
           (string
            (find-tag tag))
           ((or null (eql :non-specific))
            (language.yaml.tags::resolve-tag
             (resolver builder) tag node-path node-kind node-content)))))

  (defmethod bp:make-node ((builder tag-based-builder-mixin)
                           (kind    (eql :scalar))
                           &key
                           tag
                           content)
    (let+ (((&values tag args) (resolve-tag builder tag '() kind content))
           (node (apply #'make-node-using-tag builder kind tag
                        :content content args)))
      node))

  (defmethod bp:make-node ((builder tag-based-builder-mixin)
                           (kind    (eql :sequence))
                           &key tag)
    (let+ (((&values tag args) (resolve-tag builder tag '() kind nil))
           (node (apply #'make-node-using-tag builder kind tag args)))
      node ))

  (defmethod bp:make-node ((builder tag-based-builder-mixin)
                           (kind    (eql :mapping))
                           &key tag)
    (let+ (((&values tag args) (resolve-tag builder tag '() kind nil))
           (node (apply #'make-node-using-tag builder kind tag args)))
      node)))
