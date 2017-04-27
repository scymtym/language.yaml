;;;; types.lisp --- Test for types provided by the base module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.base.test)

(in-suite language.yaml.base)

(test node-kind.smoke
  "Smoke test for the `node-kind' type"

  (is-true (typep :scalar   'node-kind))
  (is-true (typep :sequence 'node-kind))
  (is-true (typep :mapping  'node-kind)))

;;; Scalar node styles

(test scalar-flow-style.smoke
  "Smoke test for the `scalar-flow-style' type."

  (is-true (typep :empty         'scalar-flow-style))
  (is-true (typep :plain         'scalar-flow-style))
  (is-true (typep :single-quoted 'scalar-flow-style))
  (is-true (typep :double-quoted 'scalar-flow-style)))

(test scalar-block-style.smoke
  "Smoke test for the `scalar-block-style' type."

  (is-true (typep :literal 'scalar-block-style))
  (is-true (typep :folded  'scalar-block-style)))

(test scalar-style.smoke
  "Smoke test for the `scalar-style' type."

  (is-true (typep (list :flow :empty)         'scalar-style))
  (is-true (typep (list :flow :plain)         'scalar-style))
  (is-true (typep (list :flow :single-quoted) 'scalar-style))
  (is-true (typep (list :flow :double-quoted) 'scalar-style))
  (is-true (typep (list :block :literal)      'scalar-style))
  (is-true (typep (list :block :folded)       'scalar-style)))

;;; Collection node styles

(test collection-flow-style.smoke
  "Smoke test for the `collection-flow-style' type."

  (is-true (typep :explicit    'collection-flow-style))
  (is-true (typep :single-pair 'collection-flow-style)))

(test collection-block-style.smoke
  "Smoke test for the `collection-block-style' type."

  (is-true (typep :next-line 'collection-block-style))
  (is-true (typep :in-line   'collection-block-style)))

(test collection-style.smoke
  "Smoke test for the `collection-style' type."

  (is-true (typep (list :flow :explicit)    'collection-style))
  (is-true (typep (list :flow :single-pair) 'collection-style))
  (is-true (typep (list :block :next-line)  'collection-style))
  (is-true (typep (list :block :in-line)    'collection-style)))
