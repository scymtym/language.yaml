;;;; builtin.lisp --- Builtin tags.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.tags)

;; TODO move this registry into separate file
(defvar *all-tags* (make-hash-table :test #'equal))

(defun find-tag (name &key (if-does-not-exist #'error)) ; TODO name -> uri ?
  (or (gethash name *all-tags*)
      (more-conditions:error-behavior-restart-case
          (if-does-not-exist (tag-missing-error :uri name)))))

(defun (setf find-tag) (new-value name &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (setf (gethash name *all-tags*) new-value))

(defmacro define-tag ((name kind) &key documentation)
  (check-type kind node-kind)
  (once-only (name)
    `(setf (find-tag ,name)
           (make-tag ,name ,kind ,@(when documentation
                                     `(:documentation ,documentation))))))

;;; 10.1.1.1. Generic Mapping

(define-tag ("tag:yaml.org,2002:map" :mapping)
  :documentation
  "Represents an associative container, where each key is unique in
   the association and mapped to exactly one value.

   YAML places no restrictions on the type of keys; in particular,
   they are not restricted to being scalars. Example bindings to
   native types include Perl’s hash, Python’s dictionary, and Java’s
   Hashtable.")

;;; 10.1.1.2. Generic Sequence

(define-tag ("tag:yaml.org,2002:seq" :sequence)
  :documentation
  "Represents a collection indexed by sequential integers starting
   with zero.

   Example bindings to native types include Perl’s array, Python’s
   list or tuple, and Java’s array or Vector.")

;;; 10.1.1.3. Generic String

(define-tag ("tag:yaml.org,2002:str" :scalar)
  :documentation
  "Represents a Unicode string, a sequence of zero or more Unicode
   characters.

   This type is usually bound to the native language’s string type,
   or, for languages lacking one (such as C), to a character array.")
; Canonical Form:
; The obvious.

;;; 10.2. JSON Schema

;;; 10.2.1.1. Null

(define-tag ("tag:yaml.org,2002:null" :scalar)
  :documentation
  "Represents the lack of a value.

   This is typically bound to a native null-like value (e.g., undef in
   Perl, None in Python). Note that a null is different from an empty
   string. Also, a mapping entry with some key and a null value is
   valid, and different from not having that key in the mapping.")
; Canonical Form:
; null.


;;; 10.2.1.2. Boolean

(define-tag ("tag:yaml.org,2002:bool" :scalar)
  :documentation
  "Represents a true/false value.

   In languages without a native Boolean type (such as C), is usually
   bound to a native integer type, using one for true and zero for
   false.")
; Canonical Form:
; Either true or false.

;;; 10.2.1.3. Integer

(define-tag ("tag:yaml.org,2002:int" :scalar)
  :documentation
  "Represents arbitrary sized finite mathematical integers. Scalars of
   this type should be bound to a native integer data type, if
   possible.

   Some languages (such as Perl) provide only a \"number\" type that
   allows for both integer and floating-point values. A YAML processor
   may use such a type for integers, as long as they round-trip
   properly.

   In some languages (such as C), an integer may overflow the native
   type’s storage capability. A YAML processor may reject such a value
   as an error, truncate it with a warning, or find some other manner
   to round-trip it. In general, integers representable using 32
   binary digits should safely round-trip through most systems.")
; Canonical Form:
; Decimal integer notation, with a leading \"-\" character for negative values, matching the regular expression 0 | -? [1-9] [0-9]*

;;; 10.2.1.4. Floating Point

(define-tag ("tag:yaml.org,2002:float" :scalar)
  :documentation
  "Represents an approximation to real numbers, including three
   special values (positive and negative infinity, and \"not a
   number\").

   Some languages (such as Perl) provide only a \"number\" type that
   allows for both integer and floating-point values. A YAML processor
   may use such a type for floating-point numbers, as long as they
   round-trip properly.

   Not all floating-point values can be stored exactly in any given
   native type. Hence a float value may change by \"a small amount\"
   when round-tripped. The supported range and accuracy depends on the
   implementation, though 32 bit IEEE floats should be safe. Since
   YAML does not specify a particular accuracy, using floating-point
   mapping keys requires great care and is not recommended.")
; Canonical Form:
; Either 0, .inf, -.inf, .nan, or scientific notation matching the regular expression -? [1-9] ( \. [0-9]* [1-9] )? ( e [-+] [1-9] [0-9]* )?.
