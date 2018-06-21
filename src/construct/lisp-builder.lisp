;;;; native-builder.lisp --- Turns constructs into CL objects.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.construct)

;;; Model

(defclass document-stream ()
  ((documents :type     vector
              :accessor documents
              :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defclass document ()
  ((root :accessor root)))

;;; Native builder

(defclass native-builder ()
  ((expander :initarg  :expander
             :reader   expander
             :initform (make-instance 'language.yaml.tags::standard-expander))
   (resolver :initarg  :resolver
             :reader   resolver
             :initform language.yaml.tags::*core-schema-resolver*)
   ;; State
   (anchors  :reader   anchors
             :initform (make-hash-table :test #'equal)) ; TODO store this in the document?

   ))                         ; TODO allocate the hash-table lazily?

(defun make-native-builder () ; TOOO initargs
  (make-instance 'native-builder))

(defmethod bp:relate ((builder  native-builder)
                      (relation cons)
                      (left     t)
                      (right    t)
                      &rest args &key)
  (let ((relation (bp:normalize-relation relation)))
    (apply #'bp:relate builder relation left right args)))

(defun note-node (builder anchor node)
  (when anchor
    (setf (gethash anchor (anchors builder)) node))
  node)

;;; Directive

(defmethod bp:make-node ((builder native-builder)
                         (kind    (eql :directive))
                         &rest args &key name)
  (apply #'make-directive-node builder name
         (remove-from-plist args :name)))

(defmethod make-directive-node ((builder native-builder)
                                (name    (eql :tag))
                                &key
                                handle
                                prefix)
  )

;;; Tag

(defmethod bp:make-node ((builder native-builder)
                         (kind    (eql :tag))
                         &key
                         ((:kind tag-kind))
                         prefix
                         suffix
                         content) ; TODO bounds
  (ecase tag-kind
    (:verbatim
     content)
    (:shorthand
     (language.yaml.tags:expand-shorthand (expander builder) prefix suffix))
    (:non-specific
     tag-kind)))

;;; Alias

(defmethod bp:make-node ((builder native-builder)
                         (kind    (eql :alias))
                         &key
                         anchor)
  (gethash anchor (anchors builder)))

;;; Scalar
;;;
;;; Strings are handled in the obvious way.
;;;
;;; For empty nodes, the YAML specification suggests the following:
;;;
;;;   YAML allows the node content to be omitted in many cases. Nodes
;;;   with empty content are interpreted as if they were plain scalars
;;;   with an empty value. Such nodes are commonly resolved to a
;;;   "null" value.
;;;
;;; Accordingly, we use `nil'.

(defmethod bp:node-kind ((builder native-builder)
                         (node    null))
  :scalar)

(defmethod bp:node-kind ((builder native-builder)
                         (node    string))
  :scalar)

(defmethod bp:node-initargs ((builder native-builder)
                             (node    string))
  (list :content node))

(defmethod bp:node-relations ((builder native-builder) ; TODO necessary?
                              (node    string))
  '())

(defun resolve-tag (builder tag node-path node-kind node-content)
  (etypecase tag
    (string
     (find-tag tag))
    ((or null (eql :non-specific))
     (language.yaml.tags::resolve-tag
      (resolver builder) tag node-path node-kind node-content))))

(defmethod bp:make-node ((builder native-builder)
                         (kind    (eql :scalar))
                         &key
                         tag
                         anchor
                         content)
  (when (consp content) ; TODO
    (break))
  (let-plus:let+ (((let-plus:&values tag args)
                   (resolve-tag builder tag '() kind content))
                  (node (apply #'make-node-using-tag builder kind tag
                               :content content args)))
    (note-node builder anchor node)))

;; TODO are these really called "core tags"?
(macrolet ((define-core-tag-mapping
               (tag (&optional (content-arg 'content content-arg-p)
                     &rest keyword-args)
                &body body)
             `(defmethod make-node-using-tag
                  ((builder native-builder)
                   (kind    (eql :scalar))
                   (tag     (eql (find-tag ,tag)))
                   &key ,content-arg ,@keyword-args)
                ,@(unless content-arg-p
                    `((declare (ignore ,content-arg))))
                ,@body)))

  (define-core-tag-mapping "tag:yaml.org,2002:null" ()
    nil)

  (define-core-tag-mapping "tag:yaml.org,2002:bool" (content)
    (esrap:parse '(or parser.common-rules:boolean-literal/capital-case
                      parser.common-rules:boolean-literal/lower-case)
                 content)) ; TODO may not accept TRUE

  (define-core-tag-mapping "tag:yaml.org,2002:int" (content (radix 10))
    (parse-integer content :radix radix :start (case radix
                                                 (8  2)
                                                 (16 2)
                                                 (t  0))))

  (define-core-tag-mapping "tag:yaml.org,2002:float" (content)
    (esrap:parse 'parser.common-rules:float-literal content))

  (define-core-tag-mapping "tag:yaml.org,2002:str" (content)
    content))

;;; Sequence

(defmethod bp:node-kind ((builder native-builder)
                         (node    vector))
  :sequence)

(defmethod bp:node-relations ((builder native-builder)
                              (node    vector))
  '((:entry . *)))

(defmethod bp:node-relation ((builder  native-builder)
                             (relation (eql :entry))
                             (node     vector))
  node)

#+no (defmethod bp:make-node ((builder native-builder)
                         (kind    (eql :sequence))
                         &key
                         tag
                         anchor)
  (let ((tag (resolve-tag builder tag '() kind nil)))
    (note-node builder anchor (make-array 0 :adjustable t :fill-pointer 0))))

#+no (defmethod bp:relate ((builder  native-builder)
                      (relation (eql :entry))
                      (left     vector)
                      (right    t)
                      &key)
  (vector-push-extend right left)
  left)

(defstruct (list-node
            (:constructor make-list-node ())
            (:predicate nil)
            (:copier nil))
  (elements (make-array 0 :adjustable t :fill-pointer 0) :type vector :read-only t))

(defmethod bp:make-node ((builder native-builder)
                         (kind    (eql :sequence))
                         &key
                         tag
                         anchor)
  (note-node builder anchor (make-list-node)))

(defmethod bp:relate ((builder  native-builder)
                      (relation (eql :entry))
                      (left     list-node)
                      (right    t)
                      &key)
  (vector-push-extend right (list-node-elements left))
  left)

(defmethod bp:finish-node ((builder native-builder)
                           (kind    (eql :sequence))
                           (node    list-node))
  (coerce (list-node-elements node) 'list))

;;; Mapping and key-value-pair
;;;
;;; TODO we must check for key uniqueness while building the mapping

(defmethod bp:node-kind ((builder native-builder)
                         (node    hash-table))
  :mapping)

(defmethod bp:node-relations ((builder native-builder)
                              (node    hash-table))
  '((:entry . *)))

(defmethod bp:node-relation ((builder  native-builder)
                             (relation (eql :entry))
                             (node     hash-table))
  (hash-table-alist node))

(defmethod bp:make-node ((builder native-builder)
                         (kind    (eql :mapping))
                         &key
                         tag
                         anchor)
   (let ((tag (resolve-tag builder tag '() kind nil)))
     (note-node builder anchor (make-hash-table :test #'equal))))

(defmethod bp:relate ((builder native-builder)
                      (relate  (eql :entry))
                      (left    hash-table)
                      (right   cons)
                      &key)
  (let-plus:let+ (((key . value) right))
    (setf (gethash key left) value))
  left)

;; TODO hash-table or alist? proper strategy
(defstruct (alist-node
            (:constructor make-alist-node ()))
  (elements (make-array 0 :adjustable t :fill-pointer 0)))

(defmethod bp:make-node ((builder native-builder)
                         (kind    (eql :mapping))
                         &key
                         tag
                         anchor)
  ;; this is a problem since finish-node replaces the object
  ;; but noting the node in finish-node would break recursive structures
  ;; maybe register some kind of fixup when the node is referenced?
  (note-node builder anchor (make-alist-node)))

(defmethod bp:relate ((builder native-builder)
                      (relate  (eql :entry))
                      (left    alist-node)
                      (right   cons)
                      &key)
  (vector-push-extend right (alist-node-elements left))
  left)

(defmethod bp:finish-node ((builder native-builder)
                           (kind    (eql :mapping))
                           (node    alist-node))
  (coerce (alist-node-elements node) 'list))

(defmethod bp:node-kind ((builder native-builder)
                         (node    cons))
  :key-value-pair)

(defmethod bp:node-relations ((builder native-builder)
                              (node    cons))
  '((:key . 1) (:value . 1)))

(defmethod bp:node-relation ((builder  native-builder)
                             (relation (eql :key))
                             (node     cons))
  (car node))

(defmethod bp:node-relation ((builder  native-builder)
                             (relation (eql :value))
                             (node     cons))
  (cdr node))

(defmethod bp:make-node ((builder native-builder)
                         (kind    (eql :key-value-pair))
                         &key)
  (cons nil nil))

(defmethod bp:relate ((builder native-builder)
                      (relate  (eql :key))
                      (left    cons)
                      (right   t)
                      &key)
  (setf (car left) (make-keyword (string-upcase right)))
  left)

(defmethod bp:relate ((builder native-builder)
                      (relate  (eql :value))
                      (left    cons)
                      (right   t)
                      &key)
  (setf (cdr left) right)
  left)

;;; Document

(defmethod bp:node-kind ((builder native-builder)
                         (node    document))
  :document)

(defmethod bp:node-relations ((builder native-builder)
                              (node    document))
  '((:root . 1)))

(defmethod bp:node-relation ((builder  native-builder)
                             (relation (eql :root))
                             (node     document))
  (root node))

(defmethod bp:make-node ((builder native-builder)
                         (kind    (eql :document))
                         &key)
  (make-instance 'document))

(defmethod bp:relate ((builder  native-builder)
                      (relation (eql :root))
                      (left     document)
                      (right    t)
                      &key)
  (setf (root left) right)
  left)

;;; Stream

(defmethod bp:node-kind ((builder native-builder)
                         (node    document-stream))
  :stream)

(defmethod bp:node-relations ((builder native-builder)
                              (node    document-stream))
  '((:document . *)))

(defmethod bp:node-relation ((builder  native-builder)
                             (relation (eql :document))
                             (node     document-stream))
  (documents node))

(defmethod bp:make-node ((builder native-builder)
                         (kind    (eql :stream))
                         &key)
  (make-instance 'document-stream))

(defmethod bp:relate ((builder  native-builder)
                      (relation (eql :document))
                      (left     document-stream)
                      (right    document)
                      &key)
  (vector-push-extend right (documents left))
  left)
