;;;; types.lisp --- Basic types for the language.yaml system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.base)

(deftype node-kind ()
  "Node kinds.

   See 3.2.1.1 in http://www.yaml.org/spec/1.2/spec.htm."
  '(member :scalar :sequence :mapping))

;;; Scalar node styles

(deftype scalar-flow-style ()
  "Styles for flow scalar nodes.

   See 3.2.3.1 in http://www.yaml.org/spec/1.2/spec.htm."
  '(member :empty :plain :single-quoted :double-quoted))

(deftype scalar-block-style ()
  "Styles for block scalar nodes.

   See 3.2.3.1 in http://www.yaml.org/spec/1.2/spec.htm."
  '(member :literal :folded))

(deftype scalar-style ()
  "Styles for scalar nodes.

   See 3.2.3.1 in http://www.yaml.org/spec/1.2/spec.htm."
  '(or (cons (eql :flow)  (cons scalar-flow-style null))
       (cons (eql :block) (cons scalar-block-style null))))

;;; Collection node styles

(deftype collection-flow-style ()
  "Styles for flow collection nodes.

   See 3.2.3.1 in http://www.yaml.org/spec/1.2/spec.htm."
  '(member :explicit :single-pair))

(deftype collection-block-style ()
  "Styles for block collection nodes.

   See 3.2.3.1 in http://www.yaml.org/spec/1.2/spec.htm."
  '(member :next-line :in-line))

(deftype collection-style ()
  "Styles for collection nodes.

   See 3.2.3.1 in http://www.yaml.org/spec/1.2/spec.htm."
  '(or (cons (eql :flow)  (cons collection-flow-style null))
       (cons (eql :block) (cons collection-block-style null))))
