;;;; grammar.lisp --- Grammar used by the parser module.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; This grammar implements and tries to closely follow the YAML 1.2
;;;; specification which can be obtained at
;;;; http://www.yaml.org/spec/1.2/spec.html

;;;; Note: The grammar in this file is partially based on the file
;;;; Reference.bnf which is a part of version 0.9.3 of the
;;;; YamlReference Haskell package. Parts of this grammar have been
;;;; obtained by a mostly automated translation from Haskell parser
;;;; DSL used in the YamlReference package to the esrap grammar DSL.

(cl:in-package #:language.yaml.parser)

#+esrap.grammar-class
(defgrammar #:cmake
    (:use #:whitespace
          #:literals
          #:comments))
#+esrap.grammar-class
(in-grammar #:cmake)

;;; Utility functions and rules

(defun make-scalar-node (style tag anchor content start end)
  (check-type style scalar-style)
  (node* (:scalar :style   style
                  :tag     tag
                  :anchor  anchor
                  :content content
                  :bounds  (cons start end))))

(defun make-collection-node (kind style tag anchor entries start end)
  (check-type kind  (member :sequence :mapping))
  (check-type style collection-style)
  (node* (kind :style   style
               :tag     tag
               :anchor  anchor
               :bounds  (cons start end))
    (* :entry entries)))

(defun non-empty? (result) ; TODO almost unused
  (not (emptyp result)))

(defun not-null? (thing) ; TODO get rid of this
  (not (null thing)))

(defrule start-of-line
    (or (< 1 b-break) <beginning-of-input>)
  (:constant nil))

;;; 5.1 Character Set

(defrule c-printable
    (or #\Tab #\Newline #\Return
        (character-ranges (#\  #\~))
        #\Next-Line
        (character-ranges (#\No-break_space #\UD7FF))                 ; 8 bit
        (character-ranges (#\UE000 #\Replacement_character))          ; 16 bit
        (character-ranges (#\Linear_b_syllable_b008_a #\U0010FFFF)))) ; 32 bit

(defrule nb-json
    (or #\Tab (character-ranges (#\Space #\U0010FFFF))))

;;; 5.2 Character Encodings

(defrule c-byte-order-mark
    #\zero_width_no-break_space)

;;; 5.3 Indicator Characters [4-12]

(macrolet ((define-indicator-rules (flow-specs other-specs)
             (let+ ((rule-names '())
                    ((&flet+ make-rule ((name expression))
                       (let ((rule-name (symbolicate '#:c- name)))
                         (push rule-name rule-names)
                         `(defrule ,rule-name ,expression)))))
               `(progn
                  ,@(mapcar #'make-rule flow-specs)
                  (defrule c-flow-indicator (or ,@rule-names))
                  ,@(mapcar #'make-rule other-specs)
                  (defrule c-indicator (or ,@rule-names))))))
  (define-indicator-rules
    ;; flow indicators
    ((collect-entry  #\,)
     (sequence-start #\[) (sequence-end #\])
     (mapping-start  #\{) (mapping-end  #\}))
    ;; other indicators
    ((sequence-entry #\-)
     (mapping-key #\?)    (mapping-value #\:)
     (comment #\#)
     (anchor #\&)         (alias #\*)         (tag #\!)
     (literal #\|)        (folded #\>)
     (single-quote "'")   (double-quote #\")
     (directive #\%)
     (reserved (or #\@ #\`)))))

;;; 5.4 Line Break Characters

(defrule b-line-feed
    #\Newline
  (:constant #\Newline))

(defrule b-carriage-return
    #\Return
  (:constant #\Return))

(defrule b-char
    (or b-line-feed b-carriage-return))

(defrule nb-char
    (and (! (or c-byte-order-mark b-char)) c-printable)
  (:function second))

(defrule b-break
    (or (and b-carriage-return b-line-feed)
        b-carriage-return
        b-line-feed))

(defrule b-as-line-feed
    b-break)

(defrule b-non-content
    b-break
  (:constant nil))

;;; 5.5 White Space Characters

(defrule s-space
    #\Space
  (:error-report nil))

(defrule s-tab
    #\Tab
  (:error-report nil))

(defrule s-white
    (or s-space s-tab)
  (:error-report nil))

(defrule ns-char
    (and (! s-white) nb-char)
  (:function second))

;;; 5.6 Miscellaneous Characters

(defrule ns-dec-digit
    (character-ranges (#\0 #\9)))

(defrule ns-hex-digit
    (or ns-dec-digit
        (or (character-ranges (#\A #\F)) (character-ranges (#\a #\f)))))

(defrule ns-ascii-letter
    (or (character-ranges (#\A #\Z)) (character-ranges (#\a #\z))))

(defrule ns-word-char
    (or ns-dec-digit (or ns-ascii-letter #\-)))

(defrule ns-uri-char
    (or (and #\% ns-hex-digit ns-hex-digit)
        ns-word-char
        #\# #\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\, #\_
        #\. #\! #\~ #\* #\' #\( #\) #\[ #\]))

(defrule ns-tag-char
    (and (! (and (! c-flow-indicator) c-tag)) ns-uri-char)
  (:error-report :detail))

;;; 5.7 Escaped Characters

(defrule c-escape
    "\\")

(macrolet ((define-escape-rules (&rest specs)
             (let+ ((rule-names '())
                    ((&flet+ make-rule ((name expression &rest options))
                       (let ((rule-name (symbolicate '#:ns-esc- name)))
                         (push rule-name rule-names)
                         `(defrule ,rule-name ,expression
                            ,@options)))))
               `(progn
                  ,@(mapcar #'make-rule specs)
                  (defrule c-ns-esc-char
                      (and c-escape
                           (or ,@(reverse rule-names)))
                    (:function second))))))
  (define-escape-rules
    (null                #\0              (:constant #\Nul))
    (bell                #\a              (:constant #\Bel))
    (backspace           #\b              (:constant #\Backspace))
    (horizontal-tab      (or #\t #\Tab)   (:constant #\Tab))
    (line-feed           #\n              (:constant #\Newline))
    (vertical-tab        #\v              (:constant #\Vt))
    (form-feed           #\f              (:constant #\Page))
    (carriage-return     #\r              (:constant #\Return))
    (escape              #\e              (:constant #\Escape))
    (space               #\Space          (:constant #\Space))
    (double-quote        #\"              (:constant #\"))
    (slash               #\/              (:constant #\/))
    (backslash           "\\"             (:constant "\\"))
    (next-line           #\N              (:constant #\Newline))
    (non-breaking-space  #\_              (:constant #\_))
    (line-separator      #\L              (:constant #\Line_Separator))
    (paragraph-separator #\P              (:constant #\Paragraph_Separator))
    (8-bit               (and #\x (and ns-hex-digit ns-hex-digit))
                         (:function second)
                         (:text t)
                         (:lambda (code)
                           (code-char (parse-integer code :radix 16))))
    (16-bit              (and #\u (and ns-hex-digit ns-hex-digit
                                       ns-hex-digit ns-hex-digit))
                         (:function second)
                         (:text t)
                         (:lambda (code)
                           (code-char (parse-integer code :radix 16))))
    (32-bit              (and #\U (and ns-hex-digit ns-hex-digit
                                       ns-hex-digit ns-hex-digit
                                       ns-hex-digit ns-hex-digit
                                       ns-hex-digit ns-hex-digit))
                         (:function second)
                         (:text t)
                         (:lambda (code)
                           (code-char (parse-integer code :radix 16))))))

;;; 6.1 Indentation Spaces

;; TODO macro

(defun parse-s-indent (text position end)
  (let ((n *n*))
    (if (minusp n)
        (values nil nil "Current indent is negative")
        (multiple-value-call #'values
          (parse `(and ,@(make-list n :initial-element 's-space)) #+TODO-later `(* s-space ,n ,n) text
                 :start position :end end :junk-allowed t)
          (when (zerop n) t)))))
(defrule s-indent #'parse-s-indent
  (:function length))

(defun parse-s-indent-lt (text position end)
  (if (< *n* 1)
      (values nil nil "Current indent is negative")
      (let ((n (max (1- *n*) 0))) ; TODO (< (1- *n*) 0) cannot happen
        (multiple-value-call #'values
          (parse `(or ,@(loop :for i :from n :downto 0
                           :collect `(and ,@(make-list i :initial-element 's-space))))
                 #+TODO-later `(* s-space 0 ,n)
                 text :start position :end end :junk-allowed t)
          t))))
(defrule s-indent-lt #'parse-s-indent-lt
  (:function length))

(defun parse-s-indent-le (text position end)
  (let ((n *n*))
    (if (minusp n)
        (values nil nil "Current indent is negative")
        (multiple-value-call #'values
          (parse `(or ,@(loop :for i :from n :downto 0
                           :collect `(and ,@(make-list i :initial-element 's-space))))
                                        ; `(* s-space 0 ,n)
                 text :start position :end end :junk-allowed t)
          t))))
(defrule s-indent-le #'parse-s-indent-le
  (:function length))

;;; 6.2 Separation Spaces

(defrule s-separate-in-line
    (or (+ s-white) start-of-line)
  (:constant nil))

;;; 6.3 Line Prefixes

(defun parse-s-line-prefix (text position end)
  (multiple-value-call #'values
    (parse
     (ecase *c*
       ((:block-out :block-in) 's-block-line-prefix)
       ((:flow-out :flow-in)   's-flow-line-prefix))
     text :start position :end end :junk-allowed t)
    (when (zerop *n*) t)))
(defrule s-line-prefix
    #'parse-s-line-prefix)

(defrule s-block-line-prefix
    s-indent)

(defrule s-flow-line-prefix
    (and s-indent (? s-separate-in-line)))

;;; 6.4 Empty Lines

(defrule l-empty
    (and (or s-line-prefix s-indent-lt) b-as-line-feed)
  (:constant #.(string #\Newline)))

;;; 6.5 Line Folding

(defrule b-l-trimmed
    (and b-non-content (+ l-empty))
  (:constant #.(string #\Newline)))

(defrule b-as-space
    b-break
  (:constant " "))

(defrule b-l-folded
    (or b-l-trimmed b-as-space))

(defun parse-s-flow-folded (text position end)
  (let ((*c* :flow-in))
    (parse '(and (? s-separate-in-line) b-l-folded s-flow-line-prefix)
           text :start position :end end :raw t)))
(defrule s-flow-folded
    #'parse-s-flow-folded)

;;; 6.6 Comments

(defrule c-nb-comment-text
    (and c-comment (* nb-char))
  (:function second)
  (:text t))

(defrule b-comment
    (or b-non-content <end-of-input>))

(defrule s-b-comment
    (and (? (and s-separate-in-line (? c-nb-comment-text))) b-comment)
  (:function cadar))

(defrule l-comment
    (and s-separate-in-line (? c-nb-comment-text) b-comment)
  (:destructure (separate text break)
    (declare (ignore separate))
    (or text (when (and break (not (eq break :end-of-input))) '(()))))
  (:error-report nil))

(defrule s-l-comments
    (and (or s-b-comment start-of-line) (* (not-null? l-comment))) ; TODO this is broken since l-comment can consume nothing
  (:function flatten) ; TODO processing
  (:error-report nil))

;;; 6.7 Separation Lines

(defun parse-s-separate (text position end)
  (parse
   (ecase *c*
     ((:block-out :block-in :flow-out :flow-in) 's-separate-lines)
     ((:block-key :flow-key)                    's-separate-in-line))
   text :start position :end end :raw t))
(defrule s-separate
    #'parse-s-separate
  (:error-report nil))

(defrule s-separate-lines
    (or (and s-l-comments s-flow-line-prefix) s-separate-in-line)
  (:error-report nil))

;;; 6.8 Directives

(defrule l-directive
    (and c-directive
         (or (and (and)                 (or ns-yaml-directive
                                            ns-tag-directive))
             (and (! (or "YAML" "TAG")) ns-reserved-directive))
         s-l-comments)
  (:function second)
  (:function second))

(defrule helper-directive-parameter
    (and s-separate-in-line ns-directive-parameter)
  (:function second)
  (:lambda (content &bounds start end)
    (node* (:directive-parameter :content content :bounds (cons start end)))))

(defrule ns-reserved-directive
    (and ns-directive-name (* helper-directive-parameter))
  (:destructure (name parameters &bounds start end)
    (node* (:directive :name name :kind :reserved :bounds (cons start end))
      (* :parameter parameters))))

(defrule ns-directive-name
    (+ ns-char)
  (:text t))

(defrule ns-directive-parameter
    (+ ns-char)
  (:text t))

;;; 6.8.1 Yaml Directives

(defrule ns-yaml-directive
    (and "YAML" s-separate-in-line ns-yaml-version)
  (:destructure (directive space version &bounds start end)
    (declare (ignore directive space))
    (node* (:directive :name :yaml :version version :bounds (cons start end)))))

(defrule version-digit
    (+ ns-dec-digit)
  (:function text)
  (:function parse-integer))

(defrule ns-yaml-version
    (and version-digit #\. version-digit)
  (:destructure (major dot minor)
    (declare (ignore dot))
    (cons major minor)))

;;; 6.8.2 Tag Directives

(defrule ns-tag-directive
    (and "TAG"
         s-separate-in-line c-tag-handle
         s-separate-in-line ns-tag-prefix)
  (:destructure (directive space1 handle space2 prefix)
    (declare (ignore directive space1 space2))
    (node* (:directive :name :tag :handle handle :prefix prefix))))

;;; 6.8.2.1 Tag Handles

(defrule c-tag-handle
    (or c-named-tag-handle
        c-secondary-tag-handle
        c-primary-tag-handle))

(defrule c-primary-tag-handle
    c-tag
  (:constant :primary))

(defrule c-secondary-tag-handle
    (and c-tag c-tag)
  (:constant :secondary))

(defrule c-named-tag-handle
    (and c-tag (+ ns-word-char) c-tag)
  (:function second)
  (:text t))

;;; 6.8.2.2 Tag Prefixes

(defrule ns-tag-prefix
    (or c-ns-local-tag-prefix ns-global-tag-prefix)
  (:text t))

(defrule c-ns-local-tag-prefix
    (and c-tag (* ns-uri-char))
  (:function second))

(defrule ns-global-tag-prefix
    (and ns-tag-char (* ns-uri-char)))

;;; 6.9 Node Properties

(defrule c-ns-properties/1
    (and c-ns-tag-property (? (and s-separate c-ns-anchor-property)))
  (:destructure (tag (&optional separator anchor))
    (declare (ignore separator))
    (list tag anchor)))

(defrule c-ns-properties/2
    (and c-ns-anchor-property (? (and s-separate c-ns-tag-property)))
  (:destructure (anchor (&optional separator tag))
    (declare (ignore separator))
    (list tag anchor)))

(defrule c-ns-properties
    (or c-ns-properties/1 c-ns-properties/2))

;;; 6.9.1 Node Tags

(defrule c-ns-tag-property
    (or c-verbatim-tag
        c-ns-shorthand-tag
        c-non-specific-tag))

(defun valid-verbatim-tag? (characters)
  (not (equal characters '("!"))))

(defrule verbatim-tag-content
    (valid-verbatim-tag? (+ ns-uri-char))
  (:text t))

(defrule c-verbatim-tag
    (and c-tag #\< verbatim-tag-content #\>)
  (:function third)
  (:lambda (content &bounds start end)
    (node* (:tag :kind    :verbatim
                 :content content
                 :bounds  (cons start end)))))

(defrule c-ns-shorthand-tag
    (and c-tag-handle (+ ns-tag-char))
  (:destructure (prefix suffix &bounds start end)
    (node* (:tag :kind    :shorthand
                 :prefix  prefix
                 :suffix  (esrap:text suffix) ; TODO do this in the rule
                 :bounds  (cons start end)))))

(defrule c-non-specific-tag
    (and c-tag (! (or #\< c-tag-handle)))
  (:lambda (indicator &bounds start end)
    (declare (ignore indicator))
    (node* (:tag :kind :non-specific :bounds (cons start end)))))

;;; 6.9.2 Node Anchors

(defrule c-ns-anchor-property
    (and c-anchor ns-anchor-name)
  (:function second))

(defrule ns-anchor-char
    (and (! c-flow-indicator) ns-char))

(defrule ns-anchor-name
    (+ ns-anchor-char)
  (:text t))

;;; 7.1 Alias Nodes

(defrule c-ns-alias-node
    (and c-alias ns-anchor-name)
  (:function second)
  (:lambda (anchor &bounds start end)
    (node* (:alias :anchor anchor :bounds (cons start end)))))

;;; 7.2 Empty Nodes

(defrule e-scalar
    (and)
  (:constant '(:empty . nil)))

(defrule e-node
    e-scalar
  (:lambda (content &bounds start end)
    (make-flow-node nil nil content start end)))

;;; 7.3.1 Double Quoted Style

(defrule nb-double-char
    (or (and (and)                            c-ns-esc-char)
        (and (! (or c-double-quote c-escape)) nb-json))
  (:function second))

(defrule ns-double-char
    (and (! s-white) nb-double-char)
  (:function second))

(defrule c-double-quoted
    (and c-double-quote nb-double-text c-double-quote)
  (:function second)
  (:lambda (content)
    (cons :double-quoted content)))

(defun parse-nb-double-text (text position end)
  (parse
   (ecase *c*
     ((:flow-out :flow-in)   'nb-double-multi-line)
     ((:block-key :flow-key) 'nb-double-one-line))
   text :start position :end end :raw t))
(defrule nb-double-text
    #'parse-nb-double-text)

(defrule nb-double-one-line
    (* nb-double-char)
  (:text t))

(defun parse-s-double-escaped (text position end)
  (let ((*c* :flow-in))
    (parse '(and (* s-white) c-escape b-non-content (* l-empty) s-flow-line-prefix)
           text :start position :end end :raw t)))
(defrule s-double-escaped
    #'parse-s-double-escaped)

(defrule s-double-break
    (or s-double-escaped s-flow-folded))

(defrule nb-ns-double-in-line
    (* (and (* s-white) ns-double-char))
  (:text t))

(defrule s-double-next-line
    (and s-double-break
         (? (and ns-double-char
                 nb-ns-double-in-line
                 (or s-double-next-line (* s-white))))) ; TODO make a s-white* rule
  (:function second)
  (:destructure (&optional line-first line-rest more-lines)
    (when line-first
      (list* (concatenate 'string (string line-first) line-rest) more-lines))))

(defrule nb-double-multi-line
    (and nb-ns-double-in-line (or s-double-next-line (* s-white)))
  (:destructure (first rest)
    (format nil "~A~{~%~A~}" first rest))) ; TODO good idea?

;;; 7.3.2 Single Quoted Style

(defrule c-quoted-quote
    (and c-single-quote #\'))

(defrule nb-single-char
    (or (and (and)              c-quoted-quote)
        (and (! c-single-quote) nb-json))
  (:function second))

(defrule ns-single-char
    (and (! s-white) nb-single-char)
  (:function second)
  (:text t))

(defrule c-single-quoted
    (and c-single-quote nb-single-text c-single-quote)
  (:function second)
  (:lambda (content)
    (cons :single-quoted content)))

(defun parse-nb-single-text (text position end)
  (parse (ecase *c*
           ((:flow-out :flow-in)   'nb-single-multi-line)
           ((:block-key :flow-key) 'nb-single-one-line))
         text :start position :end end :raw t))
(defrule nb-single-text
    #'parse-nb-single-text)

(defrule nb-single-one-line
    (* nb-single-char)
  (:text t))

(defrule nb-ns-single-in-line
    (* (and (* s-white) ns-single-char))
  (:text t))

(defrule s-single-next-line
    (and s-flow-folded
         (? (and ns-single-char nb-ns-single-in-line
                 (or s-single-next-line (* s-white)))))
  (:function second)
  (:destructure (&optional first second rest)
    (list* (concatenate 'string first second) rest))) ; TODO wrong for s-white case?

(defrule nb-single-multi-line
    (and nb-ns-single-in-line (or s-single-next-line (* s-white)))
  (:destructure (first rest)
    (format nil "~A~{ ~A~}" first rest)))

;;; 7.3.3 Plain Style

(defrule ns-plain-first
    (or (and (! c-indicator) ns-char)
        (and (or #\: #\? #\-) (& ns-char)))) ; spec differs in [126] here

(defun parse-ns-plain-safe (text position end)
  (parse (ecase *c*
           ((:flow-out :block-key) 'ns-plain-safe-out)
           ((:flow-in :flow-key)   'ns-plain-safe-in))
         text :start position :end end :raw t))
(defrule ns-plain-safe
    #'parse-ns-plain-safe)

(defrule ns-plain-safe-out
    (and (! (or c-comment c-mapping-value)) ns-char)
  (:function second))

(defrule ns-plain-safe-in
    (and (! c-flow-indicator) ns-plain-safe-out)
  (:function second))

(defrule ns-plain-char
    (or (and (and)         ns-plain-safe (and))
        (and (< 1 ns-char) #\#           (and))
        (and (and)         #\:           (& ns-char)))
  (:function second))

(defun parse-ns-plain (text position end)
  (parse
   (ecase *c*
     ((:flow-out :flow-in)   'ns-plain-multi-line)
     ((:block-key :flow-key) 'ns-plain-one-line))
   text :start position :end end :raw t))
(defrule ns-plain
    #'parse-ns-plain
  (:lambda (content)
    (cons :plain content)))

(defrule nb-ns-plain-in-line
    (* (and (* s-white) ns-plain-char)))

(defrule ns-plain-one-line
    (and ns-plain-first nb-ns-plain-in-line)
  (:text t))

(defrule s-ns-plain-next-line
    (and s-flow-folded ns-plain-char nb-ns-plain-in-line)
  (:function rest)
  (:text t))

(defrule ns-plain-multi-line
    (and ns-plain-one-line (* s-ns-plain-next-line))
  (:destructure (first rest)
    (format nil "~A~{~%~A~}" first rest)))

;;; 7.4 Flow Collection Styles

(defun in-flow (c)
  (ecase c
    ((:flow-out :flow-in)   :flow-in)
    ((:block-key :flow-key) :flow-key)))

;;; 7.4.1 Flow Sequences

(defun c-flow-sequence/helper (text position end)
  (let ((*c* (in-flow *c*)))
    (parse '(? ns-s-flow-seq-entries)
           text :start position :end end :raw t)))
(defrule c-flow-sequence
    (and c-sequence-start (? s-separate) #'c-flow-sequence/helper c-sequence-end)
  (:function third)
  #+no (:lambda (entries &bounds start end)
    (node* (:sequence :bounds (cons start end))
      (* :entry entries)))
  (:lambda (entries)
    (cons :sequence entries)))

(defrule ns-s-flow-seq-entries
    (and ns-flow-seq-entry (? s-separate)
         (? (and c-collect-entry (? s-separate) (? ns-s-flow-seq-entries))))
  (:destructure (first separator1 (&optional comma separator2 rest))
    (declare (ignore separator1 comma separator2))
    (list* first rest)))

(defrule ns-flow-seq-entry
    (or ns-flow-pair ns-flow-node))

;;; 7.4.2 Flow Mappings

(defun c-flow-mapping/helper (text position end)
  (let ((*c* (in-flow *c*)))
    (parse '(? ns-s-flow-map-entries)
           text :start position :end end :raw t)))
(defrule c-flow-mapping
    (and c-mapping-start (? s-separate) #'c-flow-mapping/helper c-mapping-end)
  (:function third)
  #+no (:lambda (entries &bounds start end)
    (node* (:mapping :bounds (cons start end))
      (* :entry entries)))
  (:lambda (entries)
    (cons :mapping entries)))

(defrule helper-ns-s-flow-map-entries
    (and c-collect-entry (? s-separate) (? ns-s-flow-map-entries))
  (:function third))

(defrule ns-s-flow-map-entries
    (and ns-flow-map-entry (? s-separate) (? helper-ns-s-flow-map-entries))
  (:destructure (entry separator entries)
    (declare (ignore separator))
    (list* entry entries)))

(defrule ns-flow-map-entry
    (or (and c-mapping-key s-separate ns-flow-map-explicit-entry)
        (and (and)         (and)      ns-flow-map-implicit-entry))
  (:function third))

(defrule ns-flow-map-explicit-entry
    (or ns-flow-map-implicit-entry
        (and e-node e-node)))

(defrule ns-flow-map-implicit-entry
    (or ns-flow-map-yaml-key-entry
        c-ns-flow-map-empty-key-entry
        c-ns-flow-map-json-key-entry))

(defrule ns-flow-map-yaml-key-entry
    (and ns-flow-yaml-node (or (and (? s-separate) c-ns-flow-map-separate-value)
                               (and (and)          e-node)))
  (:destructure (key (separator value) &bounds start end)
    (declare (ignore separator))
    (node* (:key-value-pair :bounds (cons start end))
      (1                               (:key   . 1) key)
      (architecture.builder-protocol:? (:value . 1) value))))

(defrule c-ns-flow-map-empty-key-entry
    (and e-node c-ns-flow-map-separate-value)
  (:destructure (key value &bounds start end)
    (node* (:key-value-pair :bounds (cons start end))
      (1 (:key   . 1) key)
      (1 (:value . 1) value))))

(defrule c-ns-flow-map-separate-value
    (and c-mapping-value (! ns-char) (or (and s-separate ns-flow-node)
                                         (and (and)      e-node)))
  (:function third)
  (:function second))

(defrule c-ns-flow-map-json-key-entry
    (and c-flow-json-node
         (or (and (? s-separate) c-ns-flow-map-adjacent-value)
             (and (and)          e-node)))
  (:destructure (key (separator value) &bounds start end)
    (declare (ignore separator))
    (node* (:key-value-pair :bounds (cons start end))
      (1                               (:key   . 1) key)
      (architecture.builder-protocol:? (:value . 1) value))))

(defrule c-ns-flow-map-adjacent-value
    (and c-mapping-value (or (and (? s-separate) ns-flow-node)
                             (and (and)          e-node)))
  (:function second)
  (:function second))

(defrule ns-flow-pair
    (or (and c-mapping-key s-separate ns-flow-map-explicit-entry)
        (and (and)         (and)      ns-flow-pair-entry))
  (:function third))

(defrule ns-flow-pair-entry
    (or ns-flow-pair-yaml-key-entry
        c-ns-flow-map-empty-key-entry
        c-ns-flow-pair-json-key-entry))

(defun ns-flow-pair-yaml-key-entry/helper (text position end)
  (let ((*c* :flow-key))
    (parse 'ns-s-implicit-yaml-key text
           :start position :end end :raw t)))
(defrule ns-flow-pair-yaml-key-entry
    (and #'ns-flow-pair-yaml-key-entry/helper
         c-ns-flow-map-separate-value)
  (:destructure (key value &bounds start end)
    (node* (:key-value-pair :bounds (cons start end))
      (1 (:key   . 1) key)
      (1 (:value . 1) value))))

(defun c-ns-flow-pair-json-key-entry/helper (text position end)
  (let ((*c* :flow-key))
    (parse 'c-s-implicit-json-key text
           :start position :end end :raw t)))
(defrule c-ns-flow-pair-json-key-entry
    (and #'c-ns-flow-pair-json-key-entry/helper
         c-ns-flow-map-adjacent-value)
  (:destructure (key value &bounds start end)
    (node* (:key-value-pair :bounds (cons start end))
      (1 (:key   . 1) key)
      (1 (:value . 1) value))))

(defconstant +implicit-key-lenght-limit+ 1024)

;; TODO (ns-flow-yaml-node na) where
;;
;; -- | @na@ is the \"non-applicable\" indentation value. We use Haskell's laziness
;; -- to verify it really is never used.
;; na :: Int
;; na = error "Accessing non-applicable indentation"
(defrule ns-s-implicit-yaml-key
    (and ns-flow-yaml-node (? s-separate-in-line)) ; TODO length limit
  (:function first))

(defun c-s-implicit-json-key/helper (text position end)
  (let+ (((&values production new-position success?)
          (parse '(and c-flow-json-node (? s-separate-in-line)) text
                 :start position :end end :junk-allowed t)))
    (cond
      ((not success?)
       (values production new-position success?))
      ((> (- (or new-position end) position) +implicit-key-lenght-limit+)
       (values nil new-position (format nil "Implicit key is longer ~
                                             than ~D character~:P."
                                        +implicit-key-lenght-limit+)))
      (t
       (values production new-position success?)))))
(defrule c-s-implicit-json-key
    #'c-s-implicit-json-key/helper
  (:function first))

;;; 7.5 Flow Nodes

(defrule ns-flow-yaml-content
    ns-plain)

(defrule c-flow-json-content
    (or c-flow-sequence
        c-flow-mapping
        c-single-quoted
        c-double-quoted))

(defrule ns-flow-content
    (or ns-flow-yaml-content
        c-flow-json-content))

(defun make-null-tag () ; TODO put this somewhere
  (node* (:tag :kind   :shorthand
               :prefix :secondary
               :suffix "null")))

(defun make-str-tag ()
  (node* (:tag :kind   :shorthand
               :prefix :secondary
               :suffix "str")))

(defun+ make-flow-node (tag anchor (style . content) start end)
  (ecase style
    (:empty
     ;; TODO adding these tags should probably not be handled here
     (make-scalar-node '(:flow :empty) (or tag (make-null-tag)) anchor content start end))
    (:plain
     (check-type content string)
     (make-scalar-node '(:flow :plain) tag anchor content start end))
    ((:single-quoted :double-quoted)
     (check-type content string)
     ;; TODO see https://github.com/eudoxia0/cl-yaml/issues/7#issuecomment-232341465
     (make-scalar-node (list :flow style) (or tag (make-str-tag)) anchor content start end))
    ((:sequence :mapping)
     (make-collection-node style '(:flow :explicit) tag anchor content start end))))

;; TODO macro for this and ns-flow-node
(defrule ns-flow-yaml-node/not-alias
    (or (and (and)               (and (and)      ns-flow-yaml-content))
        (and c-ns-properties (or (and s-separate ns-flow-yaml-content)
                                 (and (and)      e-scalar))))
  (:destructure ((&optional tag anchor) (separator content) &bounds start end)
    (declare (ignore separator))
    (when content
      (make-flow-node tag anchor content start end))))

(defrule ns-flow-yaml-node
    (or c-ns-alias-node ns-flow-yaml-node/not-alias))

(defrule c-flow-json-node
    (and (? (and c-ns-properties s-separate)) c-flow-json-content)
  (:destructure ((&optional ((&optional tag anchor)) separator) content
                 &bounds start end)
    (declare (ignore separator))
    (make-flow-node tag anchor content start end)))

(defrule c-ns-flow-node/not-alias
  (or (and (and)               (and (and)      ns-flow-content))
      (and c-ns-properties (or (and s-separate ns-flow-content)
                               (and (and)      e-scalar))))
  (:destructure ((&optional tag anchor) (separator content) &bounds start end)
    (declare (ignore separator))
    (when content
      (make-flow-node tag anchor content start end))))

(defrule ns-flow-node
    (or c-ns-alias-node c-ns-flow-node/not-alias))

;;; 8.1.1 Block Scalar Headers

(defrule helper-c-b-block-header-1
    (and (and c-indentation-indicator c-chomping-indicator)
         (& (or s-white b-char)))
  (:function first))

(defrule helper-c-b-block-header-2
    (and c-chomping-indicator c-indentation-indicator)
 (:function rotate))

(defrule c-b-block-header
    (and (or helper-c-b-block-header-1 helper-c-b-block-header-2)
         s-b-comment)
  (:function first))

;;; 8.1.1.1 Block Indentation Indicator

(defrule indentation-digit
    (and (! #\0) ns-dec-digit)
  (:function second)
  (:function digit-char-p))

(defrule c-indentation-indicator
    (or indentation-digit detect-scalar-indentation))

(defun detect-scalar-indentation/helper (text position end)
  (let ((*c* :block-in))
    (parse '(* l-empty)
           text :start position :end end :raw t)))
(defrule detect-scalar-indentation
    (& (and (* nb-char)
            (and b-non-content (? #'detect-scalar-indentation/helper))
            count-spaces))
  (:function third))

(defrule count-spaces
    (* s-space)
  (:function length)
  (:lambda (space-count)
    (max 1 (- space-count *n*))))

;;; 8.1.1.2 Chomping Indicator

(defrule c-chomping-indicator-strip
    #\-
  (:constant :strip))

(defrule c-chomping-indicator-keep
    #\+
  (:constant :keep))

(defrule c-chomping-indicator-clip
    (and)
  (:constant :clip))

(defrule c-chomping-indicator
    (or c-chomping-indicator-strip
        c-chomping-indicator-keep
        c-chomping-indicator-clip))

(defun end-block-scalar/helper (text position end)
  (declare (ignore text end))
  (values (ecase *chomping-style*
            (:strip :end-scalar) ; TODO
            (:keep  :empty)
            (:clip  :end-scalar))
          position
          t))
(defrule end-block-scalar
    #'end-block-scalar/helper)

(defun b-chomped-last/helper (text position end)
  (parse (ecase *chomping-style*
           (:strip 'b-non-content #+TODO-was '(and :empty-token :end-sclar b-non-content))
           (:keep  'b-as-line-feed #+TODO-was '(and b-as-line-feed :empty-token :end-scalar))
           (:clip  'b-as-line-feed))
         text :start position :end end :raw t))
(defrule b-chomped-last
    (or #'b-chomped-last/helper <end-of-input>))

(defun l-chomped-empty/helper (text position end)
  (parse (ecase *chomping-style*
           (:strip 'l-strip-empty)
           (:keep  'l-keep-empty)
           (:clip  'l-strip-empty))
         text :start position :end end :raw t))
(defrule l-chomped-empty
    #'l-chomped-empty/helper)

(defrule l-strip-empty
    (and (* (and s-indent-le b-non-content)) (? l-trail-comments)))

(defun l-keep-empty/helper (text position end)
  (let ((*c* :block-in))
    (parse '(* l-empty) text
           :start position :end end :raw t)))
(defrule l-keep-empty
    (and #'l-keep-empty/helper (? l-trail-comments)))

(defrule l-trail-comments
    (and s-indent-lt c-nb-comment-text b-comment (* (not-null? l-comment))))

;;; 8.1.2 Literal Style

(defun c-l+literal/helper (text position end)
  (let+ (((&values (&optional indent chomping-style) position/new)
          (parse 'c-b-block-header text
                 :start position :end end :junk-allowed t)))
    (if (eql position/new position)
        (values nil position "Failed to parse block-header")
        (let ((*n*              (+ *n* indent))
              (*chomping-style* chomping-style))
          (parse 'l-literal-content text
                 :start (or position/new end) :end end :raw t)))))
(defrule c-l+literal
    (and c-literal #'c-l+literal/helper)
  (:function second)
  (:lambda (content)
    (cons :literal content)))

(defun l-nb-literal-text/helper (text position end)
  (let ((*c* :block-in))
    (parse '(* l-empty) text :start position :end end :raw t)))
(defrule l-nb-literal-text
    (and #'l-nb-literal-text/helper s-indent (+ nb-char))
  (:function third)
  (:text t))

(defrule b-nb-literal-next
    (and b-as-line-feed l-nb-literal-text)
  (:function second))

(defrule l-literal-content
    (and (or (and l-nb-literal-text (* b-nb-literal-next) b-chomped-last)
             (and end-block-scalar))
         l-chomped-empty)
  (:function first)
  (:destructure (&optional first rest last) ; TODO handle end-block-scalar case
    (declare (ignore last))
    (case first
      ((:end-scalar :empty)
       '())
      (t
       (format nil "~A~{~%~A~}" first rest))))) ; TODO good idea?

;;; 8.1.3 Folded Style

;; TODO the whole folded style is very similar to the literal style; a
;; macro could probably go a long way here.

;; TODO similar to c-l+literal/helper
(defun c-l+folded/helper (text position end)
  (let+ (((&values (&optional indent chomping-style) position/new)
          (parse 'c-b-block-header text
                 :start position :end end :junk-allowed t)))
    (if (eql position/new position)
        (values nil position "Failed to parse block-header")
        (let ((*n*              (+ *n* indent))
              (*chomping-style* chomping-style))
          (when (= *n* 1) (break))
          (parse 'l-folded-content text
                 :start (or position/new end) :end end :raw t)))))
(defrule c-l+folded
    (and c-folded #'c-l+folded/helper)
  (:function second)
  (:lambda (content)
    (cons :folded content)))

(defrule helper-s-nb-folded-text
    (and ns-char (* nb-char))
  (:text t))

(defrule s-nb-folded-text
    (and s-indent helper-s-nb-folded-text)
  (:function second))

(defun l-nb-folded-lines/helper (text position end)
  (let ((*c* :block-in))
    (parse 'b-l-folded text
           :start position :end end :raw t)))
(defrule l-nb-folded-lines
    (and s-nb-folded-text (* (and #'l-nb-folded-lines/helper s-nb-folded-text))))

(defrule s-nb-spaced-text
    (and s-indent s-white (* nb-char))
  (:function third)
  (:text t))

(defun b-l-spaced/helper (text position end)
  (let ((*c* :block-in))
    (parse '(* l-empty) text
           :start position :end end :raw t)))
(defrule b-l-spaced
    (and b-as-line-feed #'b-l-spaced/helper))

(defrule l-nb-spaced
    (and s-nb-spaced-text b-l-spaced s-nb-spaced-text))

(defrule helper-l-nb-spaced-lines
    (and b-l-spaced s-nb-spaced-text)
  (:function second))

(defrule l-nb-spaced-lines
    (and s-nb-spaced-text (* helper-l-nb-spaced-lines))
  (:destructure (line lines)
    (list* line lines)))

(defun l-nb-same-lines/helper (text position end)
  (let ((*c* :block-in))
    (parse '(* l-empty) text
           :start position :end end :raw t)))
(defrule l-nb-same-lines
    (and #'l-nb-same-lines/helper (or l-nb-folded-lines l-nb-spaced-lines))
  (:function second))

(defrule helper-l-nb-diff-lines
    (and b-as-line-feed l-nb-same-lines)
  (:function second))

(defrule l-nb-diff-lines
    (and l-nb-same-lines (* helper-l-nb-diff-lines))
  (:destructure (line lines)
    (list* line lines)))

(defrule l-folded-content
    (and (or (and l-nb-diff-lines b-chomped-last)
             end-block-scalar)
         l-chomped-empty)
  (:function first))

;;; 8.2.1 Block Sequences

(defrule detect-collection-indentation
    (& (and (* (not-null? l-comment)) count-spaces))
  (:function second))

(defun l+block-sequence/helper (text position end)
  (let+ (((&values indent position/new)
          (parse 'detect-collection-indentation text
                 :start position :end end :junk-allowed t)))
    (if (zerop indent) ; (eql position/new position)
        (values nil position "Collection indentation expected")
        (let ((*n* (+ *n* indent)))
          (parse '(+ (and s-indent c-l-block-seq-entry)) text
                 :start (or position/new end) :end end :raw t)))))
(defrule l+block-sequence
    #'l+block-sequence/helper
  (:lambda (entries)
    (cons :sequence (mapcar #'second entries))))

(defun s-l+block-indented/helper (text position end)
  (let ((*c* :block-in))
    (parse 's-l+block-indented text
           :start position :end end :raw t)))
(defrule c-l-block-seq-entry
    (and c-sequence-entry (! ns-char) #'s-l+block-indented/helper)
  (:function third))

(defrule detect-inline-indentation
    (& count-spaces))

;; TODO similar to c-l+folded/helper
(defun s-l+block-indented/helper1 (text position end)
  (let+ (((&values indent position/new)
          (parse 'detect-inline-indentation text
                 :start position :end end :junk-allowed t)))
    (if nil ; (eql position/new position)
        (values nil position "Inline indentation expected")
        (let+ (((&values production position/new success?) ; TODO hack
                (let ((*n* indent))
                  (parse 's-indent text
                         :start (or position/new end) :end end :junk-allowed t))))
          (if (not success?)
              (values production position/new success?)
              (let+ ((*n* (+ *n* 1 indent))
                     ((&values production position/new success?)
                      (parse '(or ns-l-compact-sequence ns-l-compact-mapping) text
                             :start (or position/new end) :end end :junk-allowed t)))
                (if success?
                    (values production position/new success?)
                    (values nil        position     nil))))))))
(defun s-l+block-indented/helper2 (text position end)
  (let ((*n* (1+ *n*)))
    (parse 'unparsed text :start position :end end :junk-allowed t)))
(defrule s-l+block-indented
    (or (and (and)  (and)            #'s-l+block-indented/helper1)
        (and (and)  (and)            s-l+block-node)
        (and e-node (? s-l-comments) ; #'s-l+block-indented/helper2
             #+no (recovery (+ n 1))))
  (:function third))

(defrule helper-ns-l-compact-sequence
    (and s-indent c-l-block-seq-entry)
  (:function second))

(defrule ns-l-compact-sequence
    (and c-l-block-seq-entry (* helper-ns-l-compact-sequence))
  (:destructure (entry entries &bounds start end)
    (make-collection-node :sequence '(:block :in-line) nil nil (list* entry entries) start end)))

;;; 8.2.2 Block Mappings

(defun l+block-mapping/helper (text position end)
  (let+ (((&values indent position/new)
          (parse 'detect-collection-indentation text
                 :start position :end end :junk-allowed t)))
    (if nil #+no (eql position/new position)
        (values nil position "Collection indentation expected")
        (let ((*n* (+ *n* indent)))
          (parse '(+ (and s-indent ns-l-block-map-entry)) text
                 :start (or position/new end) :end end :raw t)))))
(defrule l+block-mapping
    #'l+block-mapping/helper
  (:lambda (entries)
    (cons :mapping (mapcar #'second entries))))     ; TODO do this in the entry rule

(defrule ns-l-block-map-entry
  (or c-l-block-map-explicit-entry ns-l-block-map-implicit-entry))

(defrule c-l-block-map-explicit-entry
    (and c-l-block-map-explicit-key
         (or l-block-map-explicit-value e-node))
  (:destructure (key value &bounds start end)
    (node* (:key-value-pair :bounds (cons start end))
      (1 (:key   . 1) key)
      (1 (:value . 1) value))))

(defun c-l-block-map-explicit-key/helper (text position end)
  (let ((*c* :block-out))
    (parse 's-l+block-indented text
           :start position :end end :raw t)))
(defrule c-l-block-map-explicit-key
    (and c-mapping-key #'c-l-block-map-explicit-key/helper))

(defun l-block-map-explicit-value/helper (text position end)
  (let ((*c* :block-out))
    (parse 's-l+block-indented text
           :start position :end end :raw t)))
(defrule l-block-map-explicit-value
    (and s-indent c-mapping-value #'l-block-map-explicit-value/helper)
  (:function third))

(defrule ns-l-block-map-implicit-entry
    (and (or ns-s-block-map-implicit-key e-node)
         c-l-block-map-implicit-value)
  (:destructure (key value &bounds start end)
    (node* (:key-value-pair :bounds (cons start end))
      (1 (:key   . 1) key)
      (1 (:value . 1) value))))

(defun ns-s-block-map-implicit-key/helper (text position end)
  (let ((*c* :block-key))
    (parse '(or c-s-implicit-json-key ns-s-implicit-yaml-key) text
           :start position :end end :raw t)))
(defrule ns-s-block-map-implicit-key
    #'ns-s-block-map-implicit-key/helper)

(defun c-l-block-map-implicit-value/helper1 (text position end)
  (let ((*c* :block-out))
    (parse 's-l+block-node text
           :start position :end end :raw t)))
(defun c-l-block-map-implicit-value/helper2 (text position end)
  (let ((*n* (1+ *n*)))
    (parse 'unparsed text
           :start position :end end :raw t)))
(defrule c-l-block-map-implicit-value
    (and c-mapping-value
         (or (and (and)  (and)        #'c-l-block-map-implicit-value/helper1)
             (and e-node s-l-comments ; #'c-l-block-map-implicit-value/helper2
                  )))
  (:function second)
  (:function third))

(defrule helper-ns-l-compact-mapping
    (and s-indent ns-l-block-map-entry)
  (:function second))

(defrule ns-l-compact-mapping
    (and ns-l-block-map-entry (* helper-ns-l-compact-mapping))
  (:destructure (entry entries &bounds start end)
    (make-collection-node :mapping '(:block :in-line) nil nil (list* entry entries) start end)))

;;; 8.2.3 Block Nodes

(defrule unparsed-line
    (and unparsed-indent (non-empty? unparsed-text) unparsed-break)
  (:function second))

(defrule unparsed
    (and (or start-of-line (and unparsed-text unparsed-break))
         (* unparsed-line))
  (:destructure (line lines)
    (node* (:unparsed :text (if line
                                (list* line lines)
                                lines)))))

(defun unparsed-indent/helper (text position end)
  (let ((n (max *n* 0)))
    (multiple-value-call #'values
      (parse `(and ,@(make-list n :initial-element 's-space)) ; TODO (* s-space ,n ,n)
             text
             :start position :end end :junk-allowed t)
      (when (zerop n) t))))
(defrule unparsed-indent #'unparsed-indent/helper)

(defrule unparsed-text
    (* (not (or <end-of-input> c-forbidden b-break)))
  (:text t))

(defrule unparsed-break
    (or <end-of-input> (& c-forbidden) b-break (and)))

(defrule s-l+block-node
    (or s-l+block-in-block s-l+flow-in-block))

(defun s-l+flow-in-block/helper (text position end)
  (let ((*n* (1+ *n*))
        (*c* :flow-out))
    (parse '(and s-separate ns-flow-node s-l-comments) text
           :start position :end end :raw t)))
(defrule s-l+flow-in-block
    #'s-l+flow-in-block/helper
  (:function second))

(defrule s-l+block-in-block
    (or s-l+block-scalar s-l+block-collection))
;; TODO wrap in node?

(defun s-l+block-scalar/helper (text position end)
  (let ((*n* (1+ *n*)))
    (parse '(and s-separate (? (and c-ns-properties s-separate))) text
           :start position :end end :raw t)))
(defrule s-l+block-scalar
    (and #'s-l+block-scalar/helper (or c-l+literal c-l+folded))
  (:destructure ((separator1 (&optional ((&optional tag anchor)) separator2))
                 (style . content)
                 &bounds start end)     ; TODO simplify destructuring
    (declare (ignore separator1 separator2))
    (make-scalar-node `(:block ,style) tag anchor content start end)))

(defun s-l+block-collection/helper1 (text position end)
  (let ((*n* (1+ *n*)))
    (parse '(? (and s-separate c-ns-properties)) text
           :start position :end end :raw t)))
(defrule s-l+block-collection/helper1
    (and #'s-l+block-collection/helper1
         (& (and s-l-comments)))
  (:function first))
(defun s-l+block-collection/helper2 (text position end)
  (let ((*n* (ecase *c*
               (:block-out (1- *n*))
               (:block-in  *n*))))
    (parse 'l+block-sequence text
           :start position :end end :raw t)))
(defrule s-l+block-collection
    (and s-l+block-collection/helper1
         s-l-comments
         (or #'s-l+block-collection/helper2 l+block-mapping))
  (:destructure ((&optional separator ((&optional tag anchor))) skippable collection
                 &bounds start end)
    (declare (ignore separator skippable))
    (let+ (((kind . entries) collection))
      (make-collection-node kind '(:block :next-line) tag anchor entries start end))))

;;; 9.1.1 Document Prefix

(defrule l-document-prefix
    (and (? c-byte-order-mark) (+ (non-empty? l-comment)))
  (:destructure (mark comments)
    (when (or mark comments)
      (list mark comments))))

;;; 9.1.2 Document Markers

(defrule c-directives-end
    "---")

(defrule c-document-end
    "...")

(defrule l-document-suffix
    (and c-document-end s-l-comments))

(defrule c-forbidden
    (and start-of-line (or c-directives-end c-document-end)
         (or b-char s-white <end-of-input>))
  (:error-report nil))

;;; 9.1.3 Bare Documents

(defun l-bare-document/helper (text position end)
  (let ((*c* :block-in)
        (*n* -1))
    (parse 's-l+block-node text :start position :end end :raw t)))
(defrule l-bare-document
    (and (! c-forbidden) #'l-bare-document/helper)
  (:function second)
  (:function list))

;;; 9.1.4 Explicit Documents

;; unparsed uses the global value of *n* which is 0
(defrule l-explicit-document
    (and c-directives-end (or l-bare-document
                              (and e-node (? s-l-comments))
                              #+later unparsed))
  #+no recovery #+no unparsed
  (:function second))

;;; 9.1.5 Directives Documents

(defrule l-directives-document
    (and (+ l-directive) l-explicit-document)
  (:destructure (directives (toplevel . emtpy-directives))
    (declare (ignore emtpy-directives))
    (list toplevel directives)))

;;; 9.2 Streams:

(defrule l-any-document
    (or l-directives-document
        l-explicit-document
        l-bare-document)
  (:destructure (root &optional directives &bounds start end)
    (node* (:document :bounds (cons start end))
      (* :directive directives)
      (1 :root      root))))

(defrule document-prefix-and-explicit-document
    (or (and (+ l-document-prefix) (? l-explicit-document))
        (and (and)                 l-explicit-document))
  (:function second)
  (:destructure (root &optional directives &bounds start end)
    (node* (:document :bounds (cons start end))
      (* :directive directives)
      (1 :root      root))))

(defrule document-suffix-and-maybe-any-document
    (and (+ l-document-suffix) (* l-document-prefix) (or <end-of-input>
                                                         l-any-document))
  (:function third))

(defrule more-documents
    (+ (or document-suffix-and-maybe-any-document
           document-prefix-and-explicit-document)))

(defrule l-yaml-stream
    (and (* l-document-prefix)
         (or <end-of-input>
             (and c-document-end (& (or b-char s-white <end-of-input>)))
             l-any-document)
         (? more-documents))
  (:destructure (prefix first-document rest-documents &bounds start end)
    (node* (:stream :bounds (cons start end))
      (* :document (remove :end-of-input
                           (list* first-document rest-documents))))))
