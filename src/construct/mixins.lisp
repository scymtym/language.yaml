;;;; mixins.lisp --- Mixins used in the construct module.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.construct)

;;; `anchor-recording-mixin'

(defclass anchor-recording-mixin ()
  ((anchors :reader   anchors ; TODO allocate the hash-table lazily?
            :initform (make-hash-table :test #'equal)) ; TODO store this in the document?
   ))

(defmethod find-anchor ((container anchor-recording-mixin)
                        (name      t)
                        &key
                        (if-does-not-exist #'error))
  (let+ (((&values node found?) (gethash name (anchors container))))
    (cond (found?
           (values node t))
          ((functionp if-does-not-exist)
           (funcall if-does-not-exist  ; TODO add a test
                    (make-condition 'anchor-missing-error :name name)))
          (t
           if-does-not-exist))))

(defmethod (setf find-anchor) ((new-value t)
                               (container anchor-recording-mixin)
                               (name      t)
                               &key
                               if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (setf (gethash name (anchors container)) new-value))

(defmethod bp:make-node ((builder anchor-recording-mixin)
                         (kind    (eql :alias))
                         &key
                         anchor)
  (find-anchor builder anchor))

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
            (tags:find-tag tag))
           ((or null (eql :non-specific))
            (tags::resolve-tag
             (resolver builder) tag node-path node-kind node-content)))))

  (defmethod bp:make-node ((builder tag-based-builder-mixin)
                           (kind    (eql :scalar))
                           &rest args &key
                           tag
                           content)
    (let+ (((&values tag tag-args) (resolve-tag builder tag '() kind content))
           (node (apply #'make-node-using-tag builder kind tag
                        :content content
                        (append tag-args (remove-from-plist args :tag)))))
      node))

  (defmethod bp:make-node ((builder tag-based-builder-mixin)
                           (kind    (eql :sequence))
                           &rest args &key tag)
    (let+ (((&values tag tag-args) (resolve-tag builder tag '() kind nil))
           (node (apply #'make-node-using-tag builder kind tag
                        (append tag-args (remove-from-plist args :tag)))))
      node))

  (defmethod bp:make-node ((builder tag-based-builder-mixin)
                           (kind    (eql :mapping))
                           &rest args &key tag)
    (let+ (((&values tag tag-args) (resolve-tag builder tag '() kind nil))
           (node (apply #'make-node-using-tag builder kind tag
                        (append tag-args (remove-from-plist args :tag)))))
      node)))
