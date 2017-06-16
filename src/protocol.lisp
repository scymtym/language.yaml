;;;; protocol.lisp --- Protocol functions used by the parser.yaml system.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml)

;;; Dump/load protocol
;;;
;;; The basic operations of a YAML processor for turning streams of
;;; characters into YAML documents and vice versa.

(defgeneric dump (thing destination &key builder)
  (:documentation
   "Dump THING into DESTINATION using builder.

    The dump operation consists of transforming THING into a tree of
    YAML nodes, serializing the tree and emitting a stream of
    characters.

    THING can be any Lisp object as long as BUILDER can handle it.

    DESTINATION can be a character output `stream' or `pathname'.

    BUILDER, if supplied, controls how objects are represented and
    recursively destructured, starting with THING."))

(defgeneric load (source &key builder rule)
  (:documentation
   "Load and return YAML information from source.

    The load operation consists of parsing the character stream,
    composing YAML nodes and finally constructing corresponding native
    representations (Lisp objects).

    SOURCE can be a `string', a character input `stream' or a
    `pathname' designating an existing file.

    BUILDER, if supplied, controls how native representations are
    constructed from YAML nodes."))

;;; Default behavior

(defmethod dump ((thing t) (destination stream)
                 &key builder)
  )

(defmethod load ((source string)
                 &key
                 (builder (make-instance 'language.yaml.construct::native-builder))
                 (rule    :root))
  (multiple-value-bind (rule post)
      (ecase rule
        (:stream   (values 'language.yaml.parser::l-yaml-stream
                           #'identity))
        (:stream2  (values 'language.yaml.parser::l-yaml-stream
                           (lambda (stream)
                             (map 'list #'language.yaml.construct::root
                                  (language.yaml.construct::documents stream)))))

        (:document (values 'language.yaml.parser::l-any-document
                           #'identity))
        (:root     (values 'language.yaml.parser::l-any-document
                           #'language.yaml.construct::root)))
    (funcall post
             (architecture.builder-protocol:with-builder (builder)
               (esrap:parse rule source)))))

(defmethod load ((source stream) &rest args &key builder rule)
  (declare (ignore builder rule))
  (apply #'load (alexandria:read-stream-content-into-string source) args))

(defmethod load ((source pathname) &rest args &key builder rule)
  (declare (ignore builder rule))
  (alexandria:with-input-from-file (stream source)
    (apply #'load stream args)))

;;; Parser protocol

#+later (defgeneric parse (source builder &key)
  (:documentation
   "Parse the content of SOURCE as \"cmake-like\" configuration options,
    construct a parse result using BUILDER and return it.

    Signal a `cmake-parse-error' when errors are encountered."))

;; Default behavior

#+later (define-condition-translating-method parse ((source t) (builder t) &key)
  ;; When an `esrap-error' is signaled, the innermost `parse' method
  ;; is specialized on `string'. It is therefore OK to assign to
  ;; SOURCE to the source slot of the condition.
  ((esrap:esrap-error cmake-parse-error
                      :var condition)
   :source   source
   :location (when (esrap:esrap-error-position condition)
               (esrap:esrap-error-position condition)))
  ((error cmake-parse-error)
   :source source))

#+later (defmethod parse ((source t) (builder t) &key)
  (error "~@<Cannot parse source ~A with builder ~A.~@:>"
         source builder))

#+later (defmethod parse ((source stream) (builder t) &key)
  (parse (read-stream-content-into-string source) builder))

#+later (defmethod parse ((source pathname) (builder t) &key)
  (parse (read-file-into-string source) builder))

#+later (defmethod parse ((source string) (builder t)
                  &key
                  (rule 'cmake))
  (let ((*builder* builder))
    (esrap:parse rule source)))
