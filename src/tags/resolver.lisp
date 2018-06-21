;;;; resolver.lisp --- Tag resolution mechanism.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.tags)

;;; `standard-expander'

(defclass standard-expander ()
  ((shorthands :initarg  :shorthands
               :reader   shorthands
               :initform (alist-hash-table
                          '(("!"  . "!")
                            ("!!" . "tag:yaml.org,2002:"))
                          :test #'equal)
               :documentation
               "Stores a mapping from shorthands to suffixes."))
  (:documentation
   "Expands shorthands like ! and !!."))

(defmethod find-shorthand ((prefix   (eql :primary))
                           (expander standard-expander)) ; TODO avoid this keyword stuff?
  (gethash "!" (shorthands expander)))

(defmethod find-shorthand ((prefix   (eql :secondary))
                           (expander standard-expander))
  (gethash "!!" (shorthands expander)))

(defmethod find-shorthand ((prefix   string)
                           (expander standard-expander))
  (gethash prefix (shorthands expander)))

(defmethod (setf find-shorthand) ((new-value string)
                                  (prefix    (eql :primary))
                                  (expander  standard-expander))
  (setf (gethash "!" (shorthands expander)) new-value))

(defmethod (setf find-shorthand) ((new-value string)
                                  (prefix    (eql :secondary))
                                  (expander  standard-expander))
  (setf (gethash "!!" (shorthands expander)) new-value))

(defmethod (setf find-shorthand) ((new-value string)
                                  (prefix    string)
                                  (expander  standard-expander))
  (setf (gethash prefix (shorthands expander)) new-value))

(flet ((expand (expander prefix suffix)
         (concatenate 'string (find-shorthand prefix expander) suffix)))
  (declare (inline expand))

  (defmethod expand-shorthand ((expander standard-expander)
                               (prefix   (eql :primary))
                               (suffix   string))
    (expand expander "!" suffix))

  (defmethod expand-shorthand ((expander standard-expander)
                               (prefix   (eql :secondary))
                               (suffix   string))
    (expand expander "!!" suffix))

  (defmethod expand-shorthand ((expander standard-expander)
                               (prefix   string)
                               (suffix   string))
    (expand expander prefix suffix)))

;;; `standard-resolver'

(defclass standard-resolver ()
  ((kind-resolvers :type    list
                   :reader  kind-resolvers
                   :writer  (setf %kind-resolvers)
                   :documentation
                   "An alist mapping node kinds to specialized resolvers.

                    The tag kinds are :scalar, :sequence and :mapping.")))

(defmethod shared-initialize :after
    ((instance   standard-resolver)
     (slot-names t)
     &key
     (kind-resolvers nil kind-resolvers-supplied?))
  (when kind-resolvers-supplied?
    (unless (and (length= 3 kind-resolvers)
                 (assoc-value kind-resolvers :scalar)
                 (assoc-value kind-resolvers :sequence)
                 (assoc-value kind-resolvers :mapping))
      (error "TODO"))
    (setf (%kind-resolvers instance) kind-resolvers)))

(defmethod resolve-tag ((resolver     standard-resolver)
                        (tag          null)
                        (node-path    t)
                        (node-kind    t)
                        (node-content t))
  (let ((resolver (assoc-value (kind-resolvers resolver) node-kind)))
    (resolve-tag resolver tag node-path node-kind node-content)))

(macrolet ((define-method (kind tag result)
             `(defmethod resolve-tag ((resolver     standard-resolver)
                                      (tag          ,tag)
                                      (node-path    t)
                                      (node-kind    (eql ,kind))
                                      (node-content t))
                (find-tag ,result))))
  (define-method :mapping  null                "tag:yaml.org,2002:map")
  (define-method :sequence null                "tag:yaml.org,2002:seq")
  (define-method :scalar   (eql :non-specific) "tag:yaml.org,2002:str") ; TODO make constants in src/base/variables.lisp
  (define-method :sequence (eql :non-specific) "tag:yaml.org,2002:seq")
  (define-method :mapping  (eql :non-specific) "tag:yaml.org,2002:map"))

(defclass regex-resolver ()
  ((rules :type    list
          :reader  rules
          :writer  (setf %rules)))
  (:default-initargs
   :rules (more-conditions:missing-required-initarg 'regex-resolver :rules)))

(defmethod shared-initialize :after ((instance   regex-resolver)
                                     (slot-names t)
                                     &key
                                     (rules nil rules-supplied?))
  (when rules-supplied?
    (setf (%rules instance)
          (map 'list (lambda+ ((pattern tag args))
                       (list (when pattern (ppcre:create-scanner pattern))
                             tag
                             args))
               rules))))

(defmethod resolve-tag ((resolver     regex-resolver)
                        (tag          t)
                        (node-path    t)
                        (node-kind    t)
                        (node-content string))
  (loop :for (scanner tag args) :in (rules resolver)
        :when (or (not scanner) (ppcre:scan scanner node-content))
        :return (values (find-tag tag) args)))

;;; 10.3.2. Tag Resolution
(defparameter *core-schema-scalar-resolution-rules*
  '(("\\A(?:|null|Null|NULL|~)\\z"                                       "tag:yaml.org,2002:null"  ())
    ("\\A(?:true|True|TRUE|false|False|FALSE)\\z"                        "tag:yaml.org,2002:bool"  ())
    ("\\A[-+]?[0-9]+\\z"                                                 "tag:yaml.org,2002:int"   (:radix 10))
    ("\\A0o[0-7]+\\z"                                                    "tag:yaml.org,2002:int"   (:radix 8))
    ("\\A0x[0-9a-fA-F]+\\z"                                              "tag:yaml.org,2002:int"   (:radix 16))
    ("\\A[-+]?(?:\\.[0-9]+|[0-9]+(:?\\.[0-9]*)?)(?:[eE][-+]?[0-9]+)?\\z" "tag:yaml.org,2002:float" ())   ; Number
    ("\\A[-+]?(?:\\.inf|\\.Inf|\\.INF)\\z"                               "tag:yaml.org,2002:float" ())   ; Infinity
    ("\\A(?:\\.nan|\\.NaN|\\.NAN)\\z"                                    "tag:yaml.org,2002:float" ())   ; Not a number
    (nil                                                                 "tag:yaml.org,2002:str"   ()))) ; Default

(defparameter *core-schema-resolver*
  (make-instance 'standard-resolver
                 :kind-resolvers `((:scalar   . ,(make-instance 'regex-resolver
                                                                :rules *core-schema-scalar-resolution-rules*))
                                   (:sequence . t)
                                   (:mapping  . t))))
