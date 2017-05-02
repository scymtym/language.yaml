;;;; types.lisp --- Types used by the parser module.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.parser)

;; TODO better names; refer to old names in docstrings
(deftype flow-state ()
  "Possible parsing contexts:

     :block-out - Outside block sequence.
     :block-in  - Inside block sequence.
     :flow-out  - Outside flow collection.
     :flow-in   - Inside flow collection.
     :block-key - Implicit block key.
     :flow-key  - Implicit flow key.

   In block styles, indentation is used to delineate structure. To
   capture human perception of indentation the rules require special
   treatment of the \"-\" character, used in block sequences. Hence in
   some cases productions need to behave differently inside block
   sequences (:block-in context) and outside them (:block-out
   context).

   In flow styles, explicit indicators are used to delineate
   structure. These styles can be viewed as the natural extension of
   JSON to cover tagged, single-quoted and plain scalars. Since the
   latter have no delineating indicators, they are subject to some
   restrictions to avoid ambiguities. These restrictions depend on
   where they appear: as implicit keys directly inside a block
   mapping (:block-key); as implicit keys inside a flow
   mapping (:flow-key); as values inside a flow collection (:flow-in);
   or as values outside one (:flow-out).

   (Note: The previous two paragraphs have been copied from Section
   4.1 of the YAML 1.2 specification with minor modifications.)"
  '(member :block-out :block-in :block-key
           :flow-out  :flow-in  :flow-key))

(deftype chomping-style ()
  "Possible block scalar chomping behaviors:

     :strip - Remove all trailing line breaks.
     :clip  - Keep first trailing line break.
     :keep  - Keep all trailing line breaks.

   ."
  '(member :strip :clip :keep))
