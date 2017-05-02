;;;; package.lisp --- Unit tests for the grammar of the parser.yaml system.
;;;;
;;;; Copyright (C) 2013, 2015, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Note: The basic idea, structure and accompanying data files (in
;;;; the data/test-cases directory) of these unit tests are based on
;;;; the version 0.9.3 of the YamlReference Haskell package.

(cl:in-package #:language.yaml.parser.test)

(in-suite language.yaml.parser)

(defun %should-fail? (spec)
  (loop with position = 0
        while (when-let ((position1 (search (coerce '(#\Newline #\!) 'string) spec :start2 position)))
                (if (not (eql position1 (search (coerce '(#\Newline #\! #\C #\o) 'string) spec :start2 position)))
                    (return t)
                    (setf position (1+ position1))))))

;;; This finds all test case files, extract test case parameters
;;; (encoded in the file names) and generates one test case for each
;;; file.
(macrolet
    ((frob ()
       (let+ ((inputs (find-data-files "input"))
              ((&flet test-case (input)
                 (let+ (((&values rule bindings tag)
                         (parse-test-file-name input))
                        (output (output-file-name input rule bindings tag))
                        (name   (apply #'symbolicate
                                       rule
                                       (format nil "~@[~{~{.~A=~A~}~}~]" bindings)
                                       (when tag (list #\. (string-upcase tag)))))
                        ((&flet make-parse-form ()
                           `(architecture.builder-protocol:with-builder ('list)
                              (parse ',rule input)))))
                   `(test ,name
                      (let (,@bindings
                            (input  (read-file-into-string ,input))
                            (output (read-file-into-string ,output)))
                        (if (%should-fail? output)
                            (signals error ,(make-parse-form))
                            (finishes ,(make-parse-form)))))))))
         `(progn ,@(mapcar #'test-case inputs)))))
  (frob))
